import R from 'ramda';
import React from 'react';

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
  const getTypeTitle = x => getType(x).title;
  const getValueCount = x => getType(x).values.length;
  const LocalNavLink = ({index}) => (<NavLink item={index} getTitle={getTypeTitle} setItem={setType} count={getValueCount(index)} />);
  return (
    <li role="presentation" className="dropdown">
      <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
        <b>View</b>: {getType(currentType).title} <span className="caret"></span>
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
  const getTypeTitle = typeIndex => getType(typeIndex).title
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
                  { R.map(x => <li key={itemKey + '.' + x.valueIndex} className="list-group-item">{x.title}</li>, item.values) }
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
  const getValueTitle = x => oneOf(maybeProp('title', groupType.values[x]), noneTitle);
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
  const getValueTitle = x => currentType.values[x].title;
  const mapProperties = R.compose(
      R.map(([t, v]) => ({ valueIndex: v, title: getType(t).values[v].title })),
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
      currentGroup: props.group
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
  render() {
    const getType = x => this.props.data.types[x];
    const getGroupTitle = x => R.isNil(x) ? noneTitle : getType(x).title;

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

        <Content {...contentInfo} />
      </div>);
  }
}
