import React from 'react';
import { connect } from 'react-redux'
import * as actions from './actions.js'
import * as reducers from './reducers.js';
import { WorkList } from './workList.js';
import { Nav } from './nav.js';

const App = ({ dispatch, view, index }) => {
  const text =
    view === reducers.viewState.loadingIndex ? 'Loading…' :
    view === reducers.viewState.workList ? `${index.works.length} Works` :
    view === reducers.viewState.typeList ? `${index.types.length} Types` :
    'Unknown view';
  const viewWorkList = () => dispatch(actions.viewWorkList());
  const viewTypeList = () => dispatch(actions.viewTypeList());
  return (
    <Nav
      title={text}
      viewWorkList={viewWorkList}
      viewTypeList={viewTypeList}
      />
  );
};

function select(state) {
  return {
    view: state.view,
    index: state.index,
  };
}

export default connect(select)(App);
