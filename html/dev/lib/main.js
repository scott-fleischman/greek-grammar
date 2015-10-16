import 'bootstrap';
import 'bootstrap/css/bootstrap.css!'

import queryString from 'query-string';
import {App} from './render.js';
import 'fetch';
import React from 'react';
import ReactDOM from 'react-dom';

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
  }
}

function mapTypes(data) {
  const types = new Map(data.types.map(x => [x.typeName, x]));
  return {
    stages: data.stages,
    types: types,
  };
}

function loadData(dataName) {
  return fetch(`data/${dataName}.json`)
    .then(checkStatus)
    .then(x => x.json())
    .then(mapTypes)
    .catch(x => console.log('Unable to load data', dataName, x));
}

function processQueryObject({ d='data' }) {
  return {
    dataName: d,
  };
}

function processQueryString(query) {
  return processQueryObject(queryString.parse(query));
  // let dataName = query.view || 'work';

  // let process = x => x;

  // if (query.where && query.whereId) {
  //   const whereName = query.where;
  //   const whereId = parseInt(query.whereId);
  //   if (!isNaN(whereId))
  //     process = R.compose(R.filter(x => x[whereName] === whereId), process);
  // }

  // const groupName = query.group || dataName;
  // process = R.compose(doGroup(groupName), process);

  // const doRender = R.curry(renderGroup)(groupName, dataName);
  // const callback = R.compose(doRender, x => ({ items: process(x[dataName]), data: x }));
}

function go() {
  const {dataName} = processQueryString(window.location.search);
  loadData([dataName]).then(data => ReactDOM.render(<App data={data} />, document.getElementById('app')));
}

go();
