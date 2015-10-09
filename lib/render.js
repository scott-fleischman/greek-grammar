import R from 'ramda';
import queryString from 'query-string';
import 'fetch';
import React from 'react';
import ReactDOM from 'react-dom';

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

function renderGroup(element, groupName, dataName, data) {
	const titleData = data.data[groupName];
	var rendered = R.map(x => (<GroupCollapse key={x.key} elementId={element.id} groupName={groupName} dataName={dataName} item={x} title={titleData[x.key].title} />), data.items);

  ReactDOM.render(<div>{rendered}</div>, element);
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

function loadCombinedData(dataNames) {
	const promises = R.map(loadData, dataNames);
	return Promise.all(promises)
		.then(values => 
			{
				let result = {};
				values.forEach(x => result[x.name] = x.items);
				return result;
			});
}

export function render() {
	const element = document.getElementById('main');

	const query = queryString.parse(window.location.search);
	let dataName = query.view || 'work';

	let process = x => x;

	if (query.where && query.whereId) {
		const whereName = query.where;
		const whereId = parseInt(query.whereId);
		if (!isNaN(whereId))
			process = R.compose(R.filter(x => x[whereName] === whereId), process);
	}

	const groupName = query.group || dataName;
	process = R.compose(doGroup(groupName), process);

	const doRender = R.curry(renderGroup)(element, groupName, dataName);
	const callback = R.compose(doRender, x => ({ items: process(x[dataName]), data: x }));
	loadCombinedData(['word','work']).then(callback);
}
