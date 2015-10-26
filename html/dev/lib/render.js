import R from 'ramda';
import React from 'react';
import FixedDataTable from 'fixed-data-table';

const Table = FixedDataTable.Table;
const Column = FixedDataTable.Column;

const NavLink = ({item, getTitle, setItem, count}) => (
  <a href="#" onClick={setItem.bind(null, item)}>{getTitle(item)} {count ? <CountBadge count={count} /> : undefined}</a>
);

const NavStages = ({stages, currentStage, getStageTitle, setStage}) => (
  <li role="presentation" className="dropdown">
    <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      <b>Stage</b>: {getStageTitle(currentStage)} <span className="caret"></span>
    </a>
    <ul className="dropdown-menu">
      { R.map(x => (
        <li key={x.stageIndex}>
          <NavLink item={x.stageIndex} getTitle={getStageTitle} setItem={setStage} />
        </li>
        ), stages)
      }
    </ul>
  </li>
);

const NavTypes = ({types, focusSource, focusResult, currentType, getType, setType}) => {
  const getTypeTitle = x => getType(x).typeTitle;
  const getValueCount = x => getType(x).values.length;
  const LocalNavLink = ({index}) => (<NavLink item={index} getTitle={getTypeTitle} setItem={setType} count={getValueCount(index)} />);
  return (
    <li role="presentation" className="dropdown">
      <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
        <b>View</b>: {getTypeTitle(currentType)} <span className="caret"></span>
      </a>
      <ul className="dropdown-menu">
        { R.flatten(
            [ !R.isNil(focusResult) ?
              ( [ <li key="focusResult" className="dropdown-header">Focus Result</li>
                , <li key={focusResult}><LocalNavLink index={focusResult} /></li>
                ]
              ) :
              [],
            , !R.isNil(focusSource) && focusSource !== focusResult ?
              ( [ <li key="focusSource" className="dropdown-header">Focus Source</li>
                , <li key={focusSource}><LocalNavLink index={focusSource} /></li>
                ]
              ) :
              [],
            , !R.isNil(oneOf(focusResult, focusSource)) ?
              ( [ <li key="separator" role="separator" className="divider"></li> ] ) :
              [],
            , R.map(x => (<li key={x}><LocalNavLink index={x} /></li>), types)
            ]
          )
        }
      </ul>
    </li>
  );
};

const NavGroups = ({groups, currentType, currentGroup, getGroupTitle, setGroup}) => (
  <li role="presentation" className="dropdown">
    <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      <b>Group By</b>: {getGroupTitle(currentGroup)}  <span className="caret"></span>
    </a>
    <ul className="dropdown-menu">
      { R.map(x => (
        <li key={currentType + '.' + x}>
          <NavLink item={x} getTitle={getGroupTitle} setItem={setGroup} />
        </li>
        ), groups)
      }
    </ul>
  </li>
);

const Nav = ({getType, getGroupTitle, stages, types, groups, currentStage, currentType, currentGroup, focusResult, focusSource, setStage, setType, setGroup}) => {
  const getTypeTitle = typeIndex => getType(typeIndex).typeTitle;
  const getStageTitle = stageIndex => getTypeTitle(stages[stageIndex].topLevelType);
  return (
    <nav className="navbar navbar-default">
      <ul className="nav nav-pills">
        <NavStages
          stages={stages}
          currentStage={currentStage}
          getStageTitle={getStageTitle}
          setStage={setStage} />

        <NavTypes
          types={types}
          currentType={currentType}
          focusResult={focusResult}
          focusSource={focusSource}
          getType={getType}
          setType={setType} />

        <NavGroups
          groups={groups}
          currentType={currentType}
          currentGroup={currentGroup}
          getGroupTitle={getGroupTitle}
          setGroup={setGroup} />
      </ul>
    </nav>);
};

const CountBadge = ({count}) => (<span className="badge">{count}</span>);

const Content = ({values, currentGroupIndex, getValueTitle, getValueCount}) => {
  const elementId = 'accordion';
  return (
    <div className="panel-group" id={elementId} role="tablist" aria-multiselectable="true">
      { R.map(item =>
        {
          const itemKey = item.key;
          const headingId = 'heading' + itemKey;
          const collapseId = 'collapse' + itemKey;
          const count = getValueCount(item);
          return (
            <div key={oneOf(currentGroupIndex, '*') + '.' + itemKey} className="panel panel-default">
              <div className="panel-heading" role="tab" id={headingId}>
                <h4 className="panel-title">
                  <a className="collapsed" role="button" data-toggle="collapse" data-parent={elementId} href={'#' + collapseId} aria-expanded="false" aria-controls={collapseId}>
                    {getValueTitle(itemKey)} {count !== 1 ? <CountBadge count={count} /> : undefined}
                  </a>
                </h4>
              </div>
              <div id={collapseId} className="panel-collapse collapse" role="tabpanel" aria-labelledby={headingId}>
                <ul className="list-group">
                  { R.map(x => <li key={itemKey + '.' + x.valueIndex} className="list-group-item">{x.valueTitle}</li>, item.values) }
                </ul>
              </div>
            </div>);
        }, values)
      }
    </div>);
};

function oneOf(...args) {
  return R.compose(R.head, R.dropWhile(R.isNil))(args);
}

function maybeProp(prop, obj) {
  return R.isNil(obj) ? undefined : R.prop(prop, obj);
}

const noneTitle = 'None';

function getGroupedContentInfo(getType, currentGroupIndex, currentTypeValues) {
  const groupByFunc = x => oneOf(x.propertyValues[currentGroupIndex], 'none');
  const groupTypeIndex = currentGroupIndex;
  const groupType = getType(groupTypeIndex);
  const getValueTitle = x => oneOf(maybeProp('valueTitle', groupType.values[x]), noneTitle);
  const doGroup = groupByFunc => R.compose(
    xs => R.map(x => ({ key: x, values: xs[x] }), R.keys(xs)),
    R.groupBy(groupByFunc)
  );
  const values = doGroup(groupByFunc)(currentTypeValues);
  return {
    values: values,
    getValueTitle: getValueTitle,
    getValueCount: x => x.values.length,
    currentGroupIndex: currentGroupIndex
  };
}

function getPropertyContentInfo(getType, currentTypeIndex, currentType) {
  const getValueTitle = x => currentType.values[x].valueTitle;
  const mapProperties = R.compose(
      R.map(([t, v]) => ({ valueIndex: v, valueTitle: getType(t).values[v].valueTitle })),
      R.toPairs
    );
  const mapValues = R.map(x => ({ key: x.valueIndex, values: mapProperties(x.propertyValues) }));
  const values = mapValues(currentType.values);
  return {
    values: values,
    getValueTitle: getValueTitle,
    getValueCount: x => 1,
    currentGroupIndex: currentTypeIndex
  };
}

export class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      currentStage: props.stage,
      currentType: props.type,
      currentGroup: props.group,
      tableWidth: 300,
      tableHeight: 300,
    };
  }
  setStage(stageIndex, event) {
    this.setState({
      currentStage: stageIndex
    });
  }
  setType(typeIndex, event) {
    this.setState({
      currentType: typeIndex
    });
  }
  setGroup(groupIndex, event) {
    this.setState({
      currentGroup: groupIndex
    });
  }
  componentDidMount() {
    this._update();
    var win = window;
    if (win.addEventListener) {
      win.addEventListener('resize', this._onResize, false);
    } else if (win.attachEvent) {
      win.attachEvent('onresize', this._onResize);
    } else {
      win.onresize = this._onResize;
    }
  }

  _onResize() {
    clearTimeout(this._updateTimer);
    this._updateTimer = setTimeout(this._update, 16);
  }

  _update() {
    var win = window;

    var widthOffset = win.innerWidth < 680 ? 0 : 240;

    this.setState({
      tableWidth: win.innerWidth,
      tableHeight: win.innerHeight - 70,
    });
  }

  render() {
    const getType = x => this.props.data.types.get(x);
    const getGroupTitle = x => R.isNil(x) ? noneTitle : getType(x).typeTitle;

    const currentStageIndex = oneOf(this.state.currentStage, 0);
    const currentStage = this.props.data.stages[currentStageIndex];

    const focusResult = currentStage.focusResultType;
    const focusSource = currentStage.focusSourceType;
    const stageTypes = R.filter(x => x !== focusResult && x !== focusSource, currentStage.allTypes);
    const currentTypeIndex = this.state.currentType && R.contains(this.state.currentType, stageTypes) ?
      this.state.currentType :
      oneOf(focusResult, focusSource, stageTypes[0]);
    const currentType = getType(currentTypeIndex);

    const typeGroups = R.concat([undefined], currentType.propertyTypes);
    const currentGroupIndex = this.state.currentGroup && R.contains(this.state.currentGroup, typeGroups) ? this.state.currentGroup : undefined;

    const contentInfo = R.isNil(currentGroupIndex) ?
      getPropertyContentInfo(getType, currentTypeIndex, currentType) :
      getGroupedContentInfo(getType, currentGroupIndex, currentType.values);

    const getValueTitle = (x, n) => getType(n).values[x].valueTitle;
    const instanceType = getType('Stage0Instance');
    const instanceValues = instanceType.values;
    const instancePropertyTypes = instanceType.propertyTypes;
    const columns = R.addIndex(R.map)((x, i) => (<Column label={x} width={200} flexGrow={1} dataKey={i} key={x} />), instancePropertyTypes);

    const myRowCount = instanceValues.length;
    const myRowGetter = x => {
      const propertyValueIndexes = instanceValues[x].propertyValues;
      const zipped = R.zip(propertyValueIndexes, instancePropertyTypes);
      return R.map(([x, n]) => getValueTitle(x, n), zipped);
    };

    return (
      <div>
        <Nav
          getType={getType}
          getGroupTitle={getGroupTitle}

          stages={this.props.data.stages}
          types={stageTypes}
          groups={typeGroups}

          currentStage={currentStageIndex}
          currentType={currentTypeIndex}
          currentGroup={currentGroupIndex}

          focusSource={focusSource}
          focusResult={focusResult}

          setStage={this.setStage.bind(this)}
          setType={this.setType.bind(this)}
          setGroup={this.setGroup.bind(this)}
        />
        <Table
          rowHeight={50}
          rowGetter={myRowGetter}
          rowsCount={myRowCount}
          width={this.state.tableWidth}
          height={this.state.tableHeight}
          headerHeight={50}>
          {columns}
        </Table>
      </div>);
  }
}
//         <Content {...contentInfo} />
