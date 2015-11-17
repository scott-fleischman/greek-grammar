import R from 'ramda';
import * as Action from "./action.js"

export const view = R.compose(R.fromPairs, R.map(x => [x,x])) ([
  'license',
  'loadingIndex',
  'loadingWork',
  'loadingType',
  'workList',
  'typeList',
  'valueList',
  'instanceList',
  'work',
  'word',
]);

export const initial = {
  visual: {
    view: view.loadingIndex,
  },
  data: {
    works: new Map(),
    types: new Map(),
  },
};

export function hasWork(state, workIndex) { return state.data.works.has(workIndex); }
export function hasValues(state, typeIndex) { return state.data.types.has(typeIndex); }

function updateMap(map, key, value) {
  const newMap = new Map(map);
  newMap.set(key, value);
  return newMap;
}

export function applyAction(state = {}, action) {
  const data = getData(state.data, action);
  const visual = getVisual(state.visual, action);
  const ephemeral = getEphemeral(action);

  return { data, visual, ephemeral };
}

export const getVisual = (state, action) => {
  switch (action.type) {
    case Action.types.viewLicense: return { view: view.license };
    case Action.types.requestIndex: return { view: view.loadingIndex };
    case Action.types.receiveIndex: return { view: view.workList };
    case Action.types.viewWorkList: return { view: view.workList };
    case Action.types.viewTypeList: return { view: view.typeList };
    case Action.types.requestWork: return { view: view.loadingWork, workIndex: action.workIndex };
    case Action.types.receiveWork: return { view: view.loadingWork, workIndex: action.workIndex };
    case Action.types.viewWork: return { view: view.work, workIndex: action.workIndex };
    case Action.types.viewWord: return { view: view.word, workIndex: action.workIndex, wordIndex: action.wordIndex };
    case Action.types.viewValueList: return { view: view.valueList, typeIndex: action.typeIndex };
    case Action.types.requestType: return { view: view.loadingType, typeIndex: action.typeIndex };
    case Action.types.receiveType: return { view: view.loadingType, typeIndex: action.typeIndex };
    case Action.types.viewInstanceList: return { view: view.instanceList, typeIndex: action.typeIndex, valueIndex: action.valueIndex };
    case Action.types.showAllLoading: return state;
    case Action.types.showAllItems: return state;
    case '@@redux/INIT': return {};
    default: console.log('Unknown action', action); return {};
  }
};

const getEphemeral = action => {
  switch (action.type) {
    case Action.types.showAllLoading: return { showAllLoading: true };
    case Action.types.showAllItems: return { showAllItems: true };
    default: return {};
  }
}

const getData = (data, action) => {
  switch (action.type) {
    case Action.types.requestIndex: return { ...data, index: { loading: true } };
    case Action.types.receiveIndex: return { ...data, index: action.index };
    case Action.types.requestWork: return { ...data, works: updateMap(data.works, action.workIndex, { loading: true }) };
    case Action.types.receiveWork: return { ...data, works: updateMap(data.works, action.workIndex, action.work) };
    case Action.types.requestType: return { ...data, types: updateMap(data.types, action.typeIndex, { loading: true }) };
    case Action.types.receiveType: return { ...data, types: updateMap(data.types, action.typeIndex, action.typeData) };
    default: return data;
  }
};

export const getActionForVisual = visual => {
  if (visual.view === view.license)
    return Action.viewLicense();
  if (visual.view === view.workList)
    return Action.fetchViewWorkList();
  if (visual.view === view.typeList)
    return Action.fetchViewTypeList();
  if (visual.view === view.work && !R.isNil(visual.workIndex))
    return Action.fetchViewWork(visual.workIndex);
  if (visual.view === view.word && !R.isNil(visual.workIndex) && !R.isNil(visual.wordIndex))
    return Action.fetchViewWord(visual.workIndex, visual.wordIndex);
  if (visual.view === view.valueList && !R.isNil(visual.typeIndex))
    return Action.fetchViewValueList(visual.typeIndex);
  if (visual.view === view.instanceList && !R.isNil(visual.typeIndex) && !R.isNil(visual.valueIndex))
    return Action.fetchViewInstanceList(visual.typeIndex, visual.valueIndex);
  else
    return undefined;
}
