import React from 'react';
import { connect } from 'react-redux'
import * as actions from './actions.js'
import * as reducers from './reducers.js';
import { WorkList } from './workList.js';
import { Nav } from './nav.js';
import R from 'ramda';

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

function getViewWorkList(works, viewWork) {
  return {
    navTitle: `${works.length} Greek Works, ${R.compose(R.sum, R.map(x => x.wordCount))(works)} Words`,
    content: (<WorkList works={works} viewWork={viewWork} />),
  };
}

function getViewTypeList(types) {
  return {
    navTitle: `${types.length} Types`,
    content: (<div></div>),
  };
}

function getViewWork(workTitle, work) {
  return {
    navTitle: workTitle,
    content: (<div>{work.workWords.length} words</div>),
  };
}

const App = ({ dispatch, view, index, workIndex, works, types }) => {
  const viewWork = x => dispatch(actions.fetchWork(x));

  let info = null;
  switch (view) {
    case reducers.viewState.loadingIndex: info = getLoadingIndex(); break;
    case reducers.viewState.loadingWork: info = getLoadingWork(workIndex, index.works); break;
    case reducers.viewState.workList: info = getViewWorkList(index.works, viewWork); break;
    case reducers.viewState.typeList: info = getViewTypeList(index.types); break;
    case reducers.viewState.work: info = getViewWork(index.works[workIndex].title, works.get(workIndex)); break;
  }
  if (!info) {
    console.log('Unknown view', view);
    return;
  }

  const viewWorkList = () => dispatch(actions.viewWorkList());
  const viewTypeList = () => dispatch(actions.viewTypeList());
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
