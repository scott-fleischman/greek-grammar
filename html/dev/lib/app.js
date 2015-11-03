import React from 'react';
import { connect } from 'react-redux'
import * as reducers from './reducers.js';

const App = ({ dispatch, view, index }) => {
	const text = view === reducers.viewState.loadingIndex ? 'Loading…' : `${index.works.length} Works`;
	return <p>{text}</p>;
};

function select(state) {
	return {
		view: state.view,
		index: state.index,
	};
}

export default connect(select)(App);
