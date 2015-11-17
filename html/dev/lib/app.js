import React from 'react';
import { connect } from 'react-redux'
import * as Action from './action.js'
import * as State from './state.js';
import { License } from './license.js';
import { Work } from './work.js';
import { Word } from './word.js';
import { WorkList } from './workList.js';
import { TypeList } from './typeList.js';
import { ValueList } from './valueList.js';
import { InstanceList } from './instanceList.js';
import { Nav } from './nav.js';
import R from 'ramda';
import QueryString from 'query-string';
import { getShowAllInfo } from './showAll.js';

function getUrl(action) {
  const visual = State.getVisual({}, action);
  const queryString = QueryString.stringify(visual);
  return `#${queryString}`;
}

export const onHashChange = dispatch => resetHash => newHash => {
  const visual = QueryString.parse(newHash);
  const action = State.getActionForVisual(visual);
  window.scrollTo(0, 0);
  if (!R.isNil(action))
    dispatch(action);
  else
    resetHash(getUrl(Action.viewWorkList()));
}

function getViewLicense() {
  return {
    navTitle: 'License',
    content: (<License />),
  };
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

function getLoadingType(typeIndex, types) {
  const text = types ? loadingItemText(types[typeIndex].title) : loadingText;
  return {
    navTitle: text,
    content: (<div></div>),
  };
}

function getViewWorkList(works, getWorkUrl) {
  return {
    navTitle: `${works.length} Greek Works, ${R.compose(R.sum, R.map(x => x.wordInfos.length))(works)} Words`,
    content: (<WorkList works={works} getWorkUrl={getWorkUrl} />),
  };
}

function getViewTypeList(types, getTypeUrl) {
  return {
    navTitle: `${types.length} Types`,
    content: (<TypeList types={types} getTypeUrl={getTypeUrl} />),
  };
}

function getViewWork(workTitle, workIndex, work, getWordUrl, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl, getShowItemInfo) {
  return {
    navTitle: workTitle,
    content: (
      <Work
        workIndex={workIndex}
        work={work}
        getWordUrl={getWordUrl}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle}
        getValueListUrl={getValueListUrl}
        getInstanceListUrl={getInstanceListUrl}
        getShowItemInfo={getShowItemInfo}
      />
    ),
  };
}

function getViewWord(workIndex, wordIndex, word, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl) {
  const wordText = getValueTitle(word[0][0], word[0][1]);
  return {
    navTitle: `Word Instance: ${wordText}`,
    content: (
      <Word
        workIndex={workIndex}
        wordIndex={wordIndex}
        word={word}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle}
        getValueListUrl={getValueListUrl}
        getInstanceListUrl={getInstanceListUrl}
      />
    ),
  };
}

function getViewValueList(values, typeTitle, typeIndex, getInstanceListUrl, getShowItemInfo) {
  return {
    navTitle: `${typeTitle}, ${values.length} Values`,
    content: (
      <ValueList
        values={values}
        typeIndex={typeIndex}
        getInstanceListUrl={getInstanceListUrl}
        getShowItemInfo={getShowItemInfo}
      />),
  };
}

function getViewInstanceList(getWorkInfo, getTypeInfo, instances, typeIndex, valueIndex, typeTitle, valueTitle, getWordUrl, getShowItemInfo) {
  return {
    navTitle: `${typeTitle}, ${valueTitle}, ${instances.length} Instances`,
    content: (
      <InstanceList
        getWorkInfo={getWorkInfo}
        getTypeInfo={getTypeInfo}
        typeIndex={typeIndex}
        valueIndex={valueIndex}
        instances={instances}
        getWordUrl={getWordUrl}
        getShowItemInfo={getShowItemInfo}
      />),
  };
}

const App = ({ dispatch, visual, data, ephemeral }) => {
  const viewWork = x => dispatch(Action.fetchViewWork(x));

  const getWorkInfo = workIndex => data.index.works[workIndex];
  const getTypeInfo = typeIndex => data.index.types[typeIndex];
  const getTypeTitle = typeIndex => getTypeInfo(typeIndex).title;
  const getValueTitle = (typeIndex, valueIndex) => getTypeInfo(typeIndex).values[valueIndex].t;

  const getInstanceListUrl = R.compose(getUrl, Action.viewInstanceList);
  const getValueListUrl = R.compose(getUrl, Action.viewValueList);
  const getWordUrl = R.compose(getUrl, Action.viewWord);
  const getShowItemInfo = (x, y) => getShowAllInfo(x, y, ephemeral.showAllLoading, ephemeral.showAllItems, () => dispatch(Action.showAll()));

  let info = null;
  switch (visual.view) {
    case State.view.license: info = getViewLicense(); break;
    case State.view.loadingIndex: info = getLoadingIndex(); break;
    case State.view.loadingWork: info = getLoadingWork(visual.workIndex, data.index.works); break;
    case State.view.loadingType: info = getLoadingType(); break;
    case State.view.workList: info = getViewWorkList(data.index.works, R.compose(getUrl, Action.viewWork)); break;
    case State.view.typeList: info = getViewTypeList(data.index.types, R.compose(getUrl, Action.viewValueList)); break;
    case State.view.work:
      info = getViewWork(
        data.index.works[visual.workIndex].title,
        visual.workIndex,
        data.works.get(visual.workIndex),
        R.compose(getUrl, Action.viewWord),
        getTypeTitle,
        getValueTitle,
        getValueListUrl,
        getInstanceListUrl,
        getShowItemInfo);
      break;
    case State.view.word:
      info = getViewWord(
        visual.workIndex,
        visual.wordIndex,
        data.works.get(visual.workIndex).words[visual.wordIndex],
        getTypeTitle,
        getValueTitle,
        getValueListUrl,
        getInstanceListUrl);
      break;
    case State.view.valueList:
      info = getViewValueList(
        getTypeInfo(visual.typeIndex).values,
        getTypeTitle(visual.typeIndex),
        visual.typeIndex,
        getInstanceListUrl,
        getShowItemInfo);
      break;
    case State.view.instanceList:
      info = getViewInstanceList(
        getWorkInfo,
        getTypeInfo,
        data.types.get(visual.typeIndex).values[visual.valueIndex].i,
        visual.typeIndex,
        visual.valueIndex,
        getTypeTitle(visual.typeIndex),
        getValueTitle(visual.typeIndex, visual.valueIndex),
        getWordUrl,
        getShowItemInfo);
      break;
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
        licenseUrl={getUrl(Action.viewLicense())}
        />
      {info.content}
    </div>
  );
};

function select(state) {
  return {
    visual: state.visual,
    data: state.data,
    ephemeral: state.ephemeral,
  };
}

export default connect(select)(App);
