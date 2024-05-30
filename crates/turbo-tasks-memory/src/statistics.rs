use std::sync::{Arc, OnceLock};

use dashmap::DashMap;
use serde::{ser::SerializeMap, Serialize, Serializer};
use turbo_tasks::{backend::PersistentTaskType, registry};

#[derive(Default, Serialize)]
struct TaskStatistics {
    /// How many times the function was executed (cache miss)
    execution_count: u32,
    /// How many times the function was read (cache hit + cache miss)
    read_count: u32,
}

static STATISTICS_BY_TASK_TYPE: OnceLock<DashMap<Arc<PersistentTaskType>, TaskStatistics>> =
    OnceLock::new();

fn with_statistics(task_type: &Arc<PersistentTaskType>, func: impl Fn(&mut TaskStatistics)) {
    let Some(all_stats) = STATISTICS_BY_TASK_TYPE.get() else {
        return;
    };
    // try a simple `get_mut` first to avoid a wasted Arc refcount when a value
    // already exists (common case), at the cost of an extra DashMap lock in the
    // case of a new entry (uncommon case)
    if let Some(mut stats) = all_stats.get_mut(task_type) {
        func(&mut *stats);
    } else {
        func(
            all_stats
                .entry(Arc::clone(task_type))
                .or_insert(TaskStatistics::default())
                .value_mut(),
        )
    }
}

pub(crate) fn increment_execution_count(task_type: &Arc<PersistentTaskType>) {
    with_statistics(task_type, |stats| stats.execution_count += 1)
}

pub(crate) fn increment_read_count(task_type: &Arc<PersistentTaskType>) {
    with_statistics(task_type, |stats| stats.read_count += 1)
}

pub fn enable_statistics() {
    let _ = STATISTICS_BY_TASK_TYPE.set(DashMap::new());
}

pub fn get_all_statistics() -> Option<AllTasksStatistics> {
    STATISTICS_BY_TASK_TYPE.get().map(AllTasksStatistics)
}

struct AllTasksStatistics(&'static DashMap<Arc<PersistentTaskType>, TaskStatistics>);

impl Serialize for AllTasksStatistics {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for entry in self.0 {
            map.serialize_entry(&registry::get_function_name(entry.key()), entry.value())?;
        }
        map.end()
    }
}
