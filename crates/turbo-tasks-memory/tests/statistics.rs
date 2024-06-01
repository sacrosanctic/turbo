#![feature(arbitrary_self_types)]

use serde_json::json;
use turbo_tasks::Vc;
use turbo_tasks_memory::{enable_statistics, get_all_statistics};
use turbo_tasks_testing::{register, run};

register!();

#[tokio::test]
async fn statistics_counts() {
    run! {
        enable_statistics();
        for i in 0..100 {
            let _ = double(i).await;
        }
        assert_eq!(serde_json::to_value(get_all_statistics()).unwrap(), json!({
            "double": {
                "execution_count": 100,
                "finished_read_count": 100,
            }
        }));
    }
}

#[turbo_tasks::function]
async fn double(val: u64) -> Vc<u64> {
    Vc::cell(val * 2)
}
