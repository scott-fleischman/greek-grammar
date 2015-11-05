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

  switch (action.type) {
    case Action.types.requestIndex: return { ...keep, view: view.loadingIndex };
    case Action.types.receiveIndex: return { ...keep, view: view.workList, index: action.index };
    case Action.types.viewWorkList: return { ...keep, view: view.workList };
    case Action.types.viewTypeList: return { ...keep, view: view.typeList };
    case Action.types.requestWork: return { ...keep, view: view.loadingWork, workIndex: action.workIndex };
    case Action.types.receiveWork: return { ...keep, view: view.loadingWork, workIndex: action.workIndex,
      works: updateMap(state.works, action.workIndex, action.work),
    };
    case Action.types.viewWork: return { ...keep, view: view.work, workIndex: action.workIndex };
    default: return state;
  }
}
