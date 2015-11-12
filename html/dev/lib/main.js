import 'bootstrap/css/bootstrap.css!'
import 'fixed-data-table/dist/fixed-data-table.css!'
import '../styles/styles.css!'

import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux'
import { createStore, applyMiddleware } from 'redux'
import thunkMiddleware from 'redux-thunk'
import * as State from './state.js'
import * as Action from './action.js'
import App, { onHashChange } from './app.js';

function go() {
  const store = applyMiddleware(thunkMiddleware)(createStore)(State.applyAction, State.initial);

  const ourOnHashChange = () => onHashChange(store.dispatch) (x => window.location.hash = x) (window.location.hash);
  window.onhashchange = ourOnHashChange;
  ourOnHashChange();

  ReactDOM.render(
    <Provider store={store}>
      <App />
    </Provider>,
    document.getElementById('app'));
}

go();
