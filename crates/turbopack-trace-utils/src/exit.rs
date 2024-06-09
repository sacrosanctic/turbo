use std::{
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex, OnceLock},
};

use anyhow::Result;
use tokio::{select, sync::mpsc, task::JoinSet};

/// A guard for the exit handler. When dropped, the exit guard will be dropped.
/// It might also be dropped on Ctrl-C.
pub struct ExitGuard<T>(Arc<Mutex<Option<T>>>);

impl<T> Drop for ExitGuard<T> {
    fn drop(&mut self) {
        drop(self.0.lock().unwrap().take())
    }
}

impl<T: Send + 'static> ExitGuard<T> {
    /// Drop a guard when Ctrl-C is pressed or the [ExitGuard] is dropped.
    pub fn new(guard: T) -> Result<Self> {
        let guard = Arc::new(Mutex::new(Some(guard)));
        {
            let guard = guard.clone();
            tokio::spawn(async move {
                tokio::signal::ctrl_c().await.unwrap();
                drop(guard.lock().unwrap().take());
                std::process::exit(0);
            });
        }
        Ok(ExitGuard(guard))
    }
}

type ExitFuture = Pin<Box<dyn Future<Output = ()> + Send + 'static>>;

/// The singular global ExitHandler. This is primarily used to ensure
/// `ExitHandler::listen` is only called once.
///
/// The global handler is intentionally not exposed, so that APIs that depend on
/// exit behavior are required to take the `ExitHandler`. This ensures that the
/// `ExitHandler` is configured before these APIs are run, and that these
/// consumers can be used with a mock instead.
static GLOBAL_EXIT_HANDLER: OnceLock<Arc<ExitHandler>> = OnceLock::new();

pub struct ExitHandler {
    tx: mpsc::UnboundedSender<ExitFuture>,
}

impl ExitHandler {
    pub fn listen() -> &'static Arc<ExitHandler> {
        let (tx, mut rx) = mpsc::unbounded_channel();
        let handler = Arc::new(ExitHandler { tx });
        if GLOBAL_EXIT_HANDLER.set(handler).is_err() {
            panic!("ExitHandler::listen must only be called once");
        }
        tokio::spawn(async move {
            tokio::signal::ctrl_c()
                .await
                .expect("failed to set ctrl_c handler");
            run_all_futures(&mut rx).await;
            std::process::exit(0);
        });
        GLOBAL_EXIT_HANDLER.get().expect("value is set")
    }

    pub fn mock() -> (Arc<ExitHandler>, ExitHandlerMocks) {
        let (tx, rx) = mpsc::unbounded_channel();
        (Arc::new(ExitHandler { tx }), ExitHandlerMocks { rx })
    }

    pub fn on_exit(&self, fut: impl Future<Output = ()> + Send + 'static) {
        // realistically, this error case can only happen with a mock
        self.tx
            .send(Box::pin(fut))
            .expect("cannot send future after process exit");
    }
}

pub struct ExitHandlerMocks {
    rx: mpsc::UnboundedReceiver<ExitFuture>,
}

impl ExitHandlerMocks {
    pub async fn mock_exit(mut self) {
        run_all_futures(&mut self.rx).await
    }
}

pub async fn run_all_futures(rx: &mut mpsc::UnboundedReceiver<ExitFuture>) {
    let mut set = JoinSet::new();
    while let Ok(fut) = rx.try_recv() {
        set.spawn(fut);
    }
    loop {
        select! {
            Some(fut) = rx.recv() => {
                set.spawn(fut);
            },
            val = set.join_next() => {
                match val {
                    Some(Ok(())) => {}
                    Some(Err(_)) => panic!("ExitHandler future panicked!"),
                    None => return,
                }
            },
        }
    }
}
