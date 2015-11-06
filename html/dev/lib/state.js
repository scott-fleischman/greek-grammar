import R from 'ramda';
import * as Action from "./action.js"

export const view = R.compose(R.fromPairs, R.map(x => [x,x])) ([
  'loadingIndex',
  'loadingWork',
  'workList',
  'typeList',
  'work',
]);

export const initial = {
  view: view.loadingIndex,
  works: new Map(),
  types: new Map(),
};

export function hasWork(state, workIndex) { return state.works.has(workIndex); }

function updateMap(map, key, value) {
  const newMap = new Map(map);
  newMap.set(key, value);
  return newMap;
}

export function applyAction(state = {}, action) {
  const cacheProperties = ['index', 'works', 'types'];
  const cache = R.pick(cacheProperties, state);

  const cacheUpdate = getCacheUpdate(cache, action);
  const stateUpdate = getStateUpdate(action);
  const view = getView(action.type);

  return { ...cache, ...cacheUpdate, ...stateUpdate, view: view };
}

const getView = actionType => {
  switch (actionType) {
    case Action.types.requestIndex: return view.loadingIndex;
    case Action.types.receiveIndex: return view.workList;
    case Action.types.viewWorkList: return view.workList;
    case Action.types.viewTypeList: return view.typeList;
    case Action.types.requestWork: return view.loadingWork;
    case Action.types.receiveWork: return view.loadingWork;
    case Action.types.viewWork: return view.work;
    default: return undefined;
  }
};

const getStateUpdate = action => {
  switch (action.type) {
    case Action.types.requestWork: return { workIndex: action.workIndex };
    case Action.types.receiveWork: return { workIndex: action.workIndex };
    case Action.types.viewWork: return { workIndex: action.workIndex };
    default: return {};
  }
};

const getCacheUpdate = (cache, action) => {
  switch (action.type) {
    case Action.types.receiveIndex: return { index: action.index };
    case Action.types.receiveWork: return { works: updateMap(cache.works, action.workIndex, action.work) };
    default: return {};
  }
};

const persistProperties = ['view', 'workIndex'];

export function getPersistedState(action) {
  const apply = f => f(action);
  const go = R.compose(R.pick(persistProperties), R.mergeAll, R.map(apply));
  return go([getView, getStateUpdate]);
}

export function getActionForPersistedState(persistedState) {
}
