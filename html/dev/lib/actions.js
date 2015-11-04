import R from 'ramda';
import 'fetch';
import * as reducers from './reducers.js';

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

export function requestIndex() { return { type: types.requestIndex }; }
export function receiveIndex(index) {
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
  return dispatchFetch('index', () => requestIndex(), x => receiveIndex(x));
}

export function viewWorkList() { return { type: types.viewWorkList }; }
export function viewTypeList() { return { type: types.viewTypeList }; }

export function requestWork(workIndex) { return { type: types.requestWork, workIndex: workIndex }; }
export function receiveWork(workIndex, work) { return { type: types.receiveWork, workIndex: workIndex, work: work }; }
export function viewWork(workIndex) { return { type: types.viewWork, workIndex: workIndex }; }

export function fetchWork(workIndex) {
  return (dispatch, getState) => {
    let initialPromise = reducers.hasWork(getState(), workIndex) ? Promise.resolve() :
      dispatchFetch(`works/work${workIndex}`, () => requestWork(workIndex), x => receiveWork(workIndex, x)) (dispatch);
    return initialPromise.then(() => dispatch(viewWork(workIndex)));
  };
}
