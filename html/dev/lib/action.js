import R from 'ramda';
import 'fetch';
import * as State from './state.js';

export const types = R.compose(R.fromPairs, R.map(x => [x,x])) ([
  'requestIndex',
  'receiveIndex',
  'viewWorkList',
  'viewTypeList',
  'requestWork',
  'receiveWork',
  'viewWork',
]);

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
  }
}

function loadData(dataName) {
  return fetch(`data/${dataName}.json`)
    .then(checkStatus)
    .then(x => x.json())
    .catch(x => console.log('Unable to load data', dataName, x));
}

function requestIndex() { return { type: types.requestIndex }; }
function receiveIndex(index) {
  return {
    type: types.receiveIndex,
    index: index,
  };
}

function dispatchFetch(data, onRequest, onResponse) {
  return dispatch => {
    dispatch(onRequest());
    return loadData(data)
      .then(x => dispatch(onResponse(x)));
  };
}

export function fetchIndex() {
  return (dispatch, getState) => {
    return getState().index ? Promise.resolve() : dispatchFetch('index', () => requestIndex(), x => receiveIndex(x)) (dispatch);
  }
}

export function viewWorkList() { return { type: types.viewWorkList }; }
export function viewTypeList() { return { type: types.viewTypeList }; }

function requestWork(workIndex) { return { type: types.requestWork, workIndex: workIndex }; }
function receiveWork(workIndex, work) { return { type: types.receiveWork, workIndex: workIndex, work: work }; }
export function viewWork(workIndex) { return { type: types.viewWork, workIndex: workIndex }; }

export function fetchViewWork(workIndex) {
  return (dispatch, getState) => {
    const dispatchViewWork = () => dispatch(viewWork(workIndex));
    if (State.hasWork(getState(), workIndex))
      return dispatchViewWork;

    return dispatchFetch(`works/work${workIndex}`, () => requestWork(workIndex), x => receiveWork(workIndex, x)) (dispatch)
      .then(dispatchViewWork);
  };
}
