import 'bootstrap/css/bootstrap.css!'
import 'fixed-data-table/dist/fixed-data-table.css!'

import {App} from './render.js';
import 'fetch';
import React from 'react';
import ReactDOM from 'react-dom';
import R from 'ramda';
import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import rootReducer from './reducers.js'

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

function processIndex({indexProperties}) {
  return {
    properties: new Map(indexProperties.map(x => [x.propertyName, x.propertyValues]))
  };
}

function go() {
  const createStoreWithMiddleware = applyMiddleware(thunkMiddleware)(createStore);
  const store = createStoreWithMiddleware(rootReducer);

  // Promise.all(R.map(loadData, ['index', 'stage0', 'works']))
  //   .then(items => {
  //     const getItem = n => R.find(x => x.kindName === n, items).kindValue;
  //     const data = {
  //       // index: processIndex(getItem('index')),
  //       // stage0: getItem('stage0'),
  //       works: getItem('works'),
  //     };
  //     ReactDOM.render(<App data={data} />, document.getElementById('app'));
  //   });
}

go();
