import R from 'ramda';
import queryString from 'query-string';
import 'fetch';
import React from 'react';
import ReactDOM from 'react-dom';

const Nav = ({stages, currentStage, getType}) => {
  const getStageTitle = stageIndex => getType(stages[stageIndex].topLevelType).title;
  return (
    <nav className="navbar navbar-default">
      <ul className="nav nav-pills">
        <li role="presentation" className="dropdown">
          <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
            <b>Stage</b>: {getStageTitle(currentStage)} <span className="caret"></span>
          </a>
          <ul className="dropdown-menu">
            {R.map(x => <li key={x.stageIndex}><a href="#">{getStageTitle(x.stageIndex)}</a></li>, stages)}
          </ul>
        </li>
      </ul>
    </nav>);
};

        // <li role="presentation" class="dropdown">
        //   <a class="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
        //     <b>View</b>: Work  <span class="caret"></span>
        //   </a>
        //   <ul class="dropdown-menu">
        //     <li class="dropdown-header">Focus</li>
        //     <li><a href="#">U.UnitUnicode <span class="badge">128,491,902</span></a></li>
        //     <li class="dropdown-header">Result</li>
        //     <li><a href="#">U.UnitUnicode <span class="badge">128,491,902</span></a></li>
        //     <li role="separator" class="divider"></li>
        //     <li><a href="#">Author <span class="badge">21</span></a></li>
        //     <li><a href="#">Word <span class="badge">124,565</span></a></li>
        //   </ul>
        // </li>
        // <li role="presentation" class="dropdown">
        //   <a class="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
        //     <b>Group By</b>: Work  <span class="caret"></span>
        //   </a>
        //   <ul class="dropdown-menu">
        //     <li><a href="?view=word&amp;group=work">Work</a></li>
        //     <li><a href="#">Source</a></li>
        //     <li><a href="#">Author</a></li>
        //   </ul>
        // </li>


const CountBadge = ({count}) => (<span className="badge">{count}</span>);
const ListGroup = ({items, render, getKey}) => (
  <ul className="list-group">
    { R.map(x => <li key={getKey(x)} className="list-group-item">{render(x)}</li>, items) }
  </ul>);
const GroupCollapse = ({elementId, groupName, dataName, item, title}) => {
  const itemIndex = item.index;
  const headingId = 'heading' + itemIndex;
  const collapseId = 'collapse' + itemIndex;
  const count = item.values.length;
  return (
    <div className="panel panel-default">
      <div className="panel-heading" role="tab" id={headingId}>
        <h4 className="panel-title">
          <a className="collapsed" role="button" data-toggle="collapse" data-parent={elementId} href={'#' + collapseId} aria-expanded="false" aria-controls={collapseId}>
            {title} {count !== 1 ? <CountBadge count={item.values.length} /> : undefined}
          </a>
        </h4>
      </div>
      <div id={collapseId} className="panel-collapse collapse" role="tabpanel" aria-labelledby={headingId}>
        <ListGroup items={item.values} render={x => x.title} getKey={x => x[dataName]} />
      </div>
    </div>);
};

function getTypeInfo(data, index) {
  return data.types[index];
}

function renderGroup(data) {
  const getType = R.curry(getTypeInfo)(data);
  ReactDOM.render(
    <Nav stages={data.stages} currentStage={0} getType={getType} />,
    document.getElementById('nav'));

  // const mainId = 'main';
  // var renderedGroups = R.map(x => (<GroupCollapse key={x.key} elementId={mainId} groupName={groupName} dataName={dataName} item={x} title={titleData[x.key].title} />), data.items);

  // ReactDOM.render(
  //   <div>{renderedGroups}</div>,
  //   document.getElementById(mainId));
}

function doGroup(groupName) {
  return R.compose(
    R.last,
    R.mapAccum((i, x) => [i + 1, { index: i, key: x.key, values: x.values } ], 0),
    xs => R.map(x => ({ key: x, values: xs[x] }), R.keys(xs)),
    R.groupBy(x => x[groupName])
    );
}

function checkStatus(response) {
  if (response.status >= 200 && response.status < 300) {
    return response;
  } else {
    const error = new Error(response.statusText);
    error.response = response;
    throw error;
  }
}

function loadData(dataName) {
  return fetch(`data/${dataName}.json`)
    .then(checkStatus)
    .then(x => x.json())
    .catch(x => console.log('Unable to load data', dataName, x));
}

export function render() {
  // const query = queryString.parse(window.location.search);
  // let dataName = query.view || 'work';

  // let process = x => x;

  // if (query.where && query.whereId) {
  //   const whereName = query.where;
  //   const whereId = parseInt(query.whereId);
  //   if (!isNaN(whereId))
  //     process = R.compose(R.filter(x => x[whereName] === whereId), process);
  // }

  // const groupName = query.group || dataName;
  // process = R.compose(doGroup(groupName), process);

  // const doRender = R.curry(renderGroup)(groupName, dataName);
  // const callback = R.compose(doRender, x => ({ items: process(x[dataName]), data: x }));
  loadData(['data']).then(renderGroup);
}
