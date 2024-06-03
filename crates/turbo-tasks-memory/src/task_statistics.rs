use std::sync::OnceLock;

use dashmap::DashMap;
use serde::{ser::SerializeMap, Serialize, Serializer};
use turbo_tasks::{backend::PersistentTaskType, registry, FunctionId, TraitTypeId};

#[derive(Default, Serialize)]
struct TaskStatistics {
    /// How many times the function began execution (roughly a cache miss)
    execution_count: u32,
    /// How many times the function was read (either from cache, or after an
    /// execution)
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

// A type implementing [`serde::Serialize`] with information about all task
// statistics.
//
// No statisitics are recorded unless [`AllTasksStatistics::enable`] is called.
#[derive(Default)]
pub struct AllTasksStatistics {
    inner: OnceLock<DashMap<TaskTypeId, TaskStatistics>>,
}

impl AllTasksStatistics {
    pub fn enable(&self) {
        // ignore error: potentially already initialized, that's okay
        let _ = self.inner.set(DashMap::new());
    }

    pub fn is_enabled(&self) -> bool {
        self.inner.get().is_some()
    }

    pub(crate) fn increment_execution_count(&self, task_type: &PersistentTaskType) {
        self.with_task_type_statistics(task_type, |stats| stats.execution_count += 1)
    }

    pub(crate) fn increment_finished_read_count(&self, task_type: &PersistentTaskType) {
        self.with_task_type_statistics(task_type, |stats| stats.finished_read_count += 1)
    }

    fn with_task_type_statistics(
        &self,
        task_type: &PersistentTaskType,
        func: impl Fn(&mut TaskStatistics),
    ) {
        if let Some(all_stats) = self.inner.get() {
            func(
                all_stats
                    .entry(task_type.into())
                    .or_insert(TaskStatistics::default())
                    .value_mut(),
            )
        };
    }
}

impl Serialize for AllTasksStatistics {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let Some(inner) = self.inner.get() else {
            return serializer.serialize_none();
        };
        let mut map = serializer.serialize_map(Some(inner.len()))?;
        for entry in inner {
            let key = match entry.key() {
                TaskTypeId::FunctionId(id) => registry::get_function_global_name(*id),
                TaskTypeId::TraitTypeId(id) => registry::get_trait_type_global_name(*id),
            };
            map.serialize_entry(key, entry.value())?;
        }
        map.end()
    }
}
