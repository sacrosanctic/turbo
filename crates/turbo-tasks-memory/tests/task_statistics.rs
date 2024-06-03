#![feature(arbitrary_self_types)]

use std::{future::Future, sync::Arc};

use anyhow::Result;
use once_cell::sync::Lazy;
use regex::Regex;
use serde_json::json;
use turbo_tasks::{TurboTasks, Vc};
use turbo_tasks_memory::MemoryBackend;
use turbo_tasks_testing::register;

register!();

#[tokio::test]
async fn test_simple_task() {
    run_with_tt(|tt| async move {
        for i in 0..10 {
            double(i).await.unwrap();
            // use cached results
            double(i).await.unwrap();
        }
        for i in 0..10 {
            double(i).await.unwrap();
        }
        assert_eq!(
            remove_hashes(serde_json::to_value(tt.backend().task_statistics()).unwrap()),
            json!({
                "turbo-tasks-memory::::double": {
                    "execution_count": 10,
                    "finished_read_count": 30,
                },
            })
        );
    })
    .await;
}

#[tokio::test]
async fn test_vc_receiving_task() {
    run_with_tt(|tt| async move {
        for i in 0..10 {
            let dvc = double(i);
            double_vc(dvc).await.unwrap();
            // use cached results
            double_vc(dvc).await.unwrap();
        }
        assert_eq!(
            remove_hashes(serde_json::to_value(tt.backend().task_statistics()).unwrap()),
            json!({
                "turbo-tasks-memory::::double": {
                    "execution_count": 10,
                    "finished_read_count": 10,
                },
                "turbo-tasks-memory::::double_vc": {
                    "execution_count": 10,
                    "finished_read_count": 20,
                },
            })
        );
    })
    .await;
}

#[tokio::test]
async fn test_trait_methods() {
    run_with_tt(|tt| async move {
        for i in 0..10 {
            let wvc = wrap(i);
            wvc.double().await.unwrap();
            wvc.double_vc().await.unwrap();
        }
        assert_eq!(
            remove_hashes(serde_json::to_value(tt.backend().task_statistics()).unwrap()),
            json!({
                "turbo-tasks-memory::::wrap": {
                    "execution_count": 10,
                    "finished_read_count": 20,
                },
                "turbo-tasks-memory::::WrappedU64::Doublable::double": {
                    "execution_count": 10,
                    "finished_read_count": 10,
                },
                "turbo-tasks-memory::::WrappedU64::Doublable::double_vc": {
                    "execution_count": 10,
                    "finished_read_count": 10,
                },
            })
        );
    })
    .await;
}

// creates Vcs, but doesn't ever execute them
#[tokio::test]
async fn test_no_execution() {
    run_with_tt(|tt| async move {
        // don't await this!
        let _ = wrap_vc(double_vc(double(123))).double().double_vc();
        assert_eq!(
            remove_hashes(serde_json::to_value(tt.backend().task_statistics()).unwrap()),
            json!({})
        );
    })
    .await;
}

// Internally, this function uses `PersistentTaskType::Native`.
#[turbo_tasks::function]
fn double(val: u64) -> Vc<u64> {
    Vc::cell(val * 2)
}

// Internally, this function uses `PersistentTaskType::ResolveNative`.
#[turbo_tasks::function]
async fn double_vc(val: Vc<u64>) -> Result<Vc<u64>> {
    let val = *val.await?;
    Ok(Vc::cell(val * 2))
}

#[turbo_tasks::value]
struct WrappedU64(u64);

#[turbo_tasks::function]
fn wrap(val: u64) -> Vc<WrappedU64> {
    WrappedU64(val).cell()
}

#[turbo_tasks::function]
async fn wrap_vc(val: Vc<u64>) -> Result<Vc<WrappedU64>> {
    Ok(WrappedU64(*val.await?).cell())
}

#[turbo_tasks::value_trait]
pub trait Doublable {
    fn double(&self) -> Vc<Self>;
    fn double_vc(self: Vc<Self>) -> Vc<Self>;
}

#[turbo_tasks::value_impl]
impl Doublable for WrappedU64 {
    #[turbo_tasks::function]
    fn double(&self) -> Vc<Self> {
        WrappedU64(self.0 * 2).cell()
    }

    #[turbo_tasks::function]
    async fn double_vc(self: Vc<Self>) -> Result<Vc<Self>> {
        let val = self.await?.0;
        Ok(WrappedU64(val * 2).cell())
    }
}

#[turbo_tasks::function]
async fn fail(val: u64) -> Result<Vc<()>> {
    anyhow::bail!("failed using {val}");
}

async fn run_with_tt<Fut>(func: impl FnOnce(Arc<TurboTasks<MemoryBackend>>) -> Fut)
where
    Fut: Future<Output = ()> + Send + 'static,
{
    *REGISTER;
    let tt = TurboTasks::new(MemoryBackend::default());
    tt.backend().task_statistics().enable();
    let fut = func(Arc::clone(&tt));
    tt.run_once(async move {
        fut.await;
        Ok(())
    })
    .await
    .unwrap();
}

// Global task identifiers can contain a hash of the crate and dependencies.
// Remove that so that we can compare against a stable value in tests.
fn remove_hashes(mut json: serde_json::Value) -> serde_json::Value {
    static HASH_RE: Lazy<Regex> = Lazy::new(|| Regex::new("@[^:]+").unwrap());
    match &mut json {
        serde_json::Value::Object(map) => {
            let old_map = std::mem::take(map);
            for (k, v) in old_map {
                map.insert(HASH_RE.replace(&k, "").into_owned(), v);
            }
        }
        _ => unreachable!("expected object"),
    };
    json
}
