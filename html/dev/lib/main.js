import 'bootstrap/css/bootstrap.css!'
import 'fixed-data-table/dist/fixed-data-table.css!'

import React from 'react';
import ReactDOM from 'react-dom';
import R from 'ramda';
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import createLogger from 'redux-logger'
import * as reducers from './reducers.js'
import * as actions from './actions.js'
import App from './app.js';

function go() {
  const loggerMiddleware = createLogger();

  const createStoreWithMiddleware = applyMiddleware(
    thunkMiddleware,
    loggerMiddleware,
  )(createStore);

  const store = createStoreWithMiddleware(reducers.root);

  store.dispatch(actions.fetchIndex());

  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.getElementById('app'));

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
