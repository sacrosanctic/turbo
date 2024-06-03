#![feature(arbitrary_self_types)]

use once_cell::sync::Lazy;
use regex::Regex;
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
        assert_eq!(remove_hashes(serde_json::to_value(get_all_statistics()).unwrap()), json!({
            "turbo_tasks_memory::::double": {
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
