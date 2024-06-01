use std::sync::{Arc, OnceLock};

use dashmap::DashMap;
use serde::{ser::SerializeMap, Serialize, Serializer};
use turbo_tasks::{backend::PersistentTaskType, registry, FunctionId, TraitTypeId};

#[derive(Default, Serialize)]
struct TaskStatistics {
    /// How many times the function was executed (roughly a cache miss)
    execution_count: u32,
    /// How many times the function was read (roughly a cache hit or a cache
    /// miss)
    finished_read_count: u32,
}

/// A function or a trait method id.
#[derive(Eq, Hash, PartialEq)]
enum TaskTypeId {
    FunctionId(FunctionId),
    TraitTypeId(TraitTypeId),
}

impl From<&PersistentTaskType> for TaskTypeId {
    fn from(value: &PersistentTaskType) -> Self {
        match value {
            PersistentTaskType::Native(id, _) | PersistentTaskType::ResolveNative(id, _) => {
                Self::FunctionId(*id)
            }
            PersistentTaskType::ResolveTrait(id, _, _) => Self::TraitTypeId(*id),
        }
    }
}

/// Initialized when [`enable_statistics`] is called.
static STATISTICS_BY_TASK_TYPE_ID: OnceLock<DashMap<TaskTypeId, TaskStatistics>> = OnceLock::new();

fn with_statistics(task_type: &Arc<PersistentTaskType>, func: impl Fn(&mut TaskStatistics)) {
    // Common case: Statistics have not been enabled (via `enable_statistics`)
    let Some(all_stats) = STATISTICS_BY_TASK_TYPE_ID.get() else {
        return;
    };
    func(
        all_stats
            .entry((&**task_type).into())
            .or_insert(TaskStatistics::default())
            .value_mut(),
    )
}

pub(crate) fn increment_execution_count(task_type: &Arc<PersistentTaskType>) {
    with_statistics(task_type, |stats| stats.execution_count += 1)
}

pub(crate) fn increment_finished_read_count(task_type: &Arc<PersistentTaskType>) {
    with_statistics(task_type, |stats| stats.finished_read_count += 1)
}

pub fn enable_statistics() {
    let _ = STATISTICS_BY_TASK_TYPE_ID.set(DashMap::new());
}

// Returns an object implementing [`serde::Serialize`] if [`enable_statistics`]
// was called.
pub fn get_all_statistics() -> Option<AllTasksStatistics> {
    STATISTICS_BY_TASK_TYPE_ID.get().map(AllTasksStatistics)
}

// A type implementing [`serde::Serialize`] with information about all task
// statistics.
pub struct AllTasksStatistics(&'static DashMap<TaskTypeId, TaskStatistics>);

impl Serialize for AllTasksStatistics {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.0.len()))?;
        for entry in self.0 {
            let key = match entry.key() {
                TaskTypeId::FunctionId(id) => registry::get_function_global_name(*id),
                TaskTypeId::TraitTypeId(id) => registry::get_trait_type_global_name(*id),
            };
            map.serialize_entry(key, entry.value())?;
        }
        map.end()
    }
}
