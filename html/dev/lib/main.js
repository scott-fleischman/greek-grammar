import 'bootstrap/css/bootstrap.css!'
import 'fixed-data-table/dist/fixed-data-table.css!'
import '../css/styles.css!'

import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import createLogger from 'redux-logger'
import * as State from './state.js'
import * as Action from './action.js'
import App, { onHashChange } from './app.js';

function go() {
  const loggerMiddleware = createLogger();

  const createStoreWithMiddleware = applyMiddleware(
    thunkMiddleware,
    loggerMiddleware,
  )(createStore);

  const store = createStoreWithMiddleware(State.applyAction, State.initial);

  store.dispatch(Action.fetchViewWorkList());

  window.onhashchange = () => onHashChange(window.location.hash);

  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.getElementById('app'));
}

go();
