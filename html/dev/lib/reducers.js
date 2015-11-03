import R from 'ramda';
import * as actions from "./actions.js"
import { combineReducers } from 'redux'

export const viewState = R.compose(R.fromPairs, R.map(x => [x,x])) ([
	'loadingIndex',
	'workList',
	'typeList',
]);

export function root(state = {}, action) {
	switch (action.type) {
		case actions.types.requestIndex: return { view: viewState.loadingIndex };
		case actions.types.receiveIndex: return { view: viewState.workList, index: action.index };
		case actions.types.viewWorkList: return { view: viewState.workList, index: state.index };
		case actions.types.viewTypeList: return { view: viewState.typeList, index: state.index };
		default: return state;
	}
}
