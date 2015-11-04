import R from 'ramda';
import * as actions from "./actions.js"
import { combineReducers } from 'redux'

export const viewState = R.compose(R.fromPairs, R.map(x => [x,x])) ([
	'loadingIndex',
	'loadingWork',
	'workList',
	'typeList',
	'work',
]);

export const initialState = {
	view: viewState.loadingIndex,
	works: new Map(),
	types: new Map(),
};

export function hasWork(state, workIndex) { return state.works.has(workIndex); }
function updateMap(map, key, value) {
	const newMap = new Map(map);
	newMap.set(key, value);
	return newMap;
}

export function root(state = {}, action) {
	switch (action.type) {
		case actions.types.requestIndex: return { ...state, view: viewState.loadingIndex };
		case actions.types.receiveIndex: return { ...state, view: viewState.workList, index: action.index };
		case actions.types.viewWorkList: return { ...state, view: viewState.workList };
		case actions.types.viewTypeList: return { ...state, view: viewState.typeList };
		case actions.types.requestWork: return { ...state, view: viewState.loadingWork, workIndex: action.workIndex };
		case actions.types.receiveWork: return { ...state, view: viewState.loadingWork, workIndex: action.workIndex,
			works: updateMap(state.works, action.workIndex, action.work),
		};
		case actions.types.viewWork: return { ...state, view: viewState.work, workIndex: action.workIndex };
		default: return state;
	}
}
