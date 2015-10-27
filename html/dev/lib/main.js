import 'bootstrap';
import 'bootstrap/css/bootstrap.css!'
import 'fixed-data-table/dist/fixed-data-table.css!'

import queryString from 'query-string';
import {App} from './render.js';
import 'fetch';
import React from 'react';
import ReactDOM from 'react-dom';
import R from 'ramda';

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

function processIndex({indexProperties}) {
  return {
    properties: new Map(indexProperties.map(x => [x.propertyName, x.propertyValues]))
  };
}

function go() {
  // const {dataName} = processQueryString(window.location.search);
  Promise.all(R.map(loadData, ['index', 'stage0', 'works']))
    .then(items => {
      const getItem = n => R.find(x => x.kindName === n, items).kindValue;
      const data = {
        index: processIndex(getItem('index')),
        stage0: getItem('stage0'),
        works: getItem('works'),
      };
      ReactDOM.render(<App data={data} />, document.getElementById('app'));
    });
}

go();
