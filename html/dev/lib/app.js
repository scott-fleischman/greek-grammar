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
  const visual = State.getVisual(action);
  const queryString = QueryString.stringify(visual);
  return `#${queryString}`;
}

export function onHashChange(newHash) {
  const visual = QueryString.parse(newHash);
  const action = State.getActionForPersistedState(visual);
  console.log('visual', visual, 'action', action);
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

const App = ({ dispatch, visual, data }) => {
  const viewWork = x => dispatch(Action.fetchWork(x));

  let info = null;
  switch (visual.view) {
    case State.view.loadingIndex: info = getLoadingIndex(); break;
    case State.view.loadingWork: info = getLoadingWork(visual.workIndex, data.index.works); break;
    case State.view.workList: info = getViewWorkList(data.index.works, viewWork, R.compose(getUrl, Action.viewWork)); break;
    case State.view.typeList: info = getViewTypeList(data.index.types); break;
    case State.view.work: info = getViewWork(data.index.works[visual.workIndex].title, visual.workIndex, data.works.get(visual.workIndex)); break;
  }
  if (!info) {
    console.log('Unknown view', visual.view);
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
    visual: state.visual,
    data: state.data,
  };
}

export default connect(select)(App);
