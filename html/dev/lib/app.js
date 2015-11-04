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

function getViewWorkList(works) {
  return {
    navTitle: `${works.length} Greek Works, ${R.compose(R.sum, R.map(x => x.wordCount))(works)} Words`,
    content: (<WorkList works={works} />),
  };
}

function getViewTypeList(types) {
  return {
    navTitle: `${types.length} Types`,
    content: (<div></div>),
  };
}

const App = ({ dispatch, view, index }) => {
  let info = null;
  switch (view) {
    case reducers.viewState.loadingIndex: info = getLoadingIndex(); break;
    case reducers.viewState.workList: info = getViewWorkList(index.works); break;
    case reducers.viewState.typeList: info = getViewTypeList(index.types); break;
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
  };
}

export default connect(select)(App);
