import React from 'react';
import { connect } from 'react-redux'
import * as Action from './action.js'
import * as State from './state.js';
import { WorkList } from './workList.js';
import { Nav } from './nav.js';
import R from 'ramda';
import { Work } from './render.js';
import QueryString from 'query-string';

function getUrl(action) {
  const persistedState = State.getPersistedState(action);
  const queryString = QueryString.stringify(persistedState);
  return `#${queryString}`;
}

export function onHashChange(newHash) {
  const parsed = QueryString.parse(newHash);
  const persistedState = State.getActionForPersistedState(parsed);
  console.log('parsed', parsed, 'persistedState', persistedState);
}

function getLoadingIndex() {
  return {
    navTitle: 'Loading…',
    content: (<div></div>),
  };
}

function getLoadingWork(workIndex, works) {
  return {
    navTitle: `Loading work ${works[workIndex].title} …`,
    content: (<div></div>),
  };
}

function getViewWorkList(works, viewWork, getViewWorkUrl) {
  return {
    navTitle: `${works.length} Greek Works, ${R.compose(R.sum, R.map(x => x.wordCount))(works)} Words`,
    content: (<WorkList works={works} viewWork={viewWork} getViewWorkUrl={getViewWorkUrl} />),
  };
}

function getViewTypeList(types) {
  return {
    navTitle: `${types.length} Types`,
    content: (<div></div>),
  };
}

function getViewWork(workTitle, workIndex, work) {
  return {
    navTitle: workTitle,
    content: (<Work workIndex={workIndex} work={work} />),
  };
}

const App = ({ dispatch, view, index, workIndex, works, types }) => {
  const viewWork = x => dispatch(Action.fetchWork(x));

  let info = null;
  switch (view) {
    case State.view.loadingIndex: info = getLoadingIndex(); break;
    case State.view.loadingWork: info = getLoadingWork(workIndex, index.works); break;
    case State.view.workList: info = getViewWorkList(index.works, viewWork, R.compose(getUrl, Action.viewWork)); break;
    case State.view.typeList: info = getViewTypeList(index.types); break;
    case State.view.work: info = getViewWork(index.works[workIndex].title, workIndex, works.get(workIndex)); break;
  }
  if (!info) {
    console.log('Unknown view', view);
    return;
  }

  const viewWorkList = () => dispatch(Action.viewWorkList());
  const viewTypeList = () => dispatch(Action.viewTypeList());
  return (
    <div>
      <Nav
        title={info.navTitle}
        viewWorkList={viewWorkList}
        viewTypeList={viewTypeList}
        />
      {info.content}
    </div>
  );
};

function select(state) {
  return {
    view: state.view,
    index: state.index,
    workIndex: state.workIndex,
    works: state.works,
    types: state.types,
  };
}

export default connect(select)(App);
