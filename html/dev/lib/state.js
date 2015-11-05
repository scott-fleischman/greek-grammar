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
  const keepProperties = ['index', 'works', 'types'];
  const keep = R.pick(keepProperties, state);

  const stateUpdate = getStateUpdate(state, action);
  const view = getView(action.type);

  return { ...keep, ...stateUpdate, view: view };
}

function getView(actionType) {
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
}

function getStateUpdate(state, action) {
  switch (action.type) {
    case Action.types.receiveIndex: return { index: action.index };
    case Action.types.requestWork: return { workIndex: action.workIndex };
    case Action.types.receiveWork: return { workIndex: action.workIndex, works: updateMap(state.works, action.workIndex, action.work) };
    case Action.types.viewWork: return { workIndex: action.workIndex };
    default: return {};
  }
}

const persistProperties = ['view', 'workIndex'];

function persistAction(action) {
  return R.compose(R.mergeAll, R.map(R.pick(persistProperties)))([state, action]);
}
