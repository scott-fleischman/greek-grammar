import R from 'ramda';
import 'fetch';

export const types = R.compose(R.fromPairs, R.map(x => [x,x])) ([
  'requestIndex',
  'receiveIndex',
  'viewWorkList',
  'viewTypeList',
  'requestWork',
  'receiveWork',
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

export function fetchIndex() {
  return dispatch => {
    dispatch(requestIndex());
    return loadData('index')
      .then(index => dispatch(receiveIndex(index)));
  };
}

export function viewWorkList() { return { type: types.viewWorkList }; }
export function viewTypeList() { return { type: types.viewTypeList }; }
