import React from 'react';
import { connect } from 'react-redux'
import * as Action from './action.js'
import * as State from './state.js';
import { WorkList } from './workList.js';
import { TypeList } from './typeList.js';
import { ValueList } from './valueList.js';
import { Nav } from './nav.js';
import R from 'ramda';
import { Work } from './render.js';
import QueryString from 'query-string';

function getUrl(action) {
  const visual = State.getVisual(action);
  const queryString = QueryString.stringify(visual);
  return `#${queryString}`;
}

export const onHashChange = dispatch => resetHash => newHash => {
  const visual = QueryString.parse(newHash);
  const action = State.getActionForVisual(visual);
  if (!R.isNil(action))
    dispatch(action);
  else
    resetHash(getUrl(Action.viewWorkList()));
}

const loadingText = 'Loading…';
const loadingItemText = item => `Loading ${item} …`

function getLoadingIndex() {
  return {
    navTitle: loadingText,
    content: (<div></div>),
  };
}

function getLoadingWork(workIndex, works) {
  const text = works ? loadingItemText(works[workIndex].title) : loadingText;
  return {
    navTitle: text,
    content: (<div></div>),
  };
}

function getLoadingValues() {
  return {
    navTitle: loadingText,
    content: (<div></div>),
  };
}

function getViewWorkList(works, getWorkUrl) {
  return {
    navTitle: `${works.length} Greek Works, ${R.compose(R.sum, R.map(x => x.wordCount))(works)} Words`,
    content: (<WorkList works={works} getWorkUrl={getWorkUrl} />),
  };
}

function getViewTypeList(types, getTypeUrl) {
  return {
    navTitle: `${types.length} Types`,
    content: (<TypeList types={types} getTypeUrl={getTypeUrl} />),
  };
}

function getViewWork(workTitle, workIndex, work) {
  return {
    navTitle: workTitle,
    content: (<Work workIndex={workIndex} work={work} />),
  };
}

function getViewValueList(values, typeTitle, typeIndex, getValueUrl) {
  return {
    navTitle: `${typeTitle}, ${values.length} Values`,
    content: (<ValueList values={values} typeIndex={typeIndex} getValueUrl={getValueUrl} />),
  };
}

const App = ({ dispatch, visual, data }) => {
  const viewWork = x => dispatch(Action.fetchViewWork(x));

  let info = null;
  switch (visual.view) {
    case State.view.loadingIndex: info = getLoadingIndex(); break;
    case State.view.loadingWork: info = getLoadingWork(visual.workIndex, data.index.works); break;
    case State.view.loadingValues: info = getLoadingValues(); break;
    case State.view.workList: info = getViewWorkList(data.index.works, R.compose(getUrl, Action.viewWork)); break;
    case State.view.typeList: info = getViewTypeList(data.index.types, R.compose(getUrl, Action.viewValueList)); break;
    case State.view.work: info = getViewWork(data.index.works[visual.workIndex].title, visual.workIndex, data.works.get(visual.workIndex)); break;
    case State.view.valueList: info = getViewValueList(data.types.get(visual.typeIndex).values, data.index.types[visual.typeIndex].title, visual.typeIndex, () => '#'); break;
  }
  if (!info) {
    console.log('Unknown view', visual);
    return (<div style={{margin:'1em'}}>Whoopsie! Something went wrong.</div>);
  }

  return (
    <div>
      <Nav
        title={info.navTitle}
        workListUrl={getUrl(Action.viewWorkList())}
        typeListUrl={getUrl(Action.viewTypeList())}
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
