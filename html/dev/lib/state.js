import R from 'ramda';
import * as actions from "./actions.js"

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
  switch (action.type) {
    case actions.types.requestIndex: return { ...state, view: view.loadingIndex };
    case actions.types.receiveIndex: return { ...state, view: view.workList, index: action.index };
    case actions.types.viewWorkList: return { ...state, view: view.workList };
    case actions.types.viewTypeList: return { ...state, view: view.typeList };
    case actions.types.requestWork: return { ...state, view: view.loadingWork, workIndex: action.workIndex };
    case actions.types.receiveWork: return { ...state, view: view.loadingWork, workIndex: action.workIndex,
      works: updateMap(state.works, action.workIndex, action.work),
    };
    case actions.types.viewWork: return { ...state, view: view.work, workIndex: action.workIndex };
    default: return state;
  }
}
