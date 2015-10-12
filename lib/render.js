import R from 'ramda';
import React from 'react';

const NavStages = ({stages, currentStage, getStageTitle, setStage}) => (
  <li role="presentation" className="dropdown">
    <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      <b>Stage</b>: {getStageTitle(currentStage)} <span className="caret"></span>
    </a>
    <ul className="dropdown-menu">
      { R.map(x => (
        <li key={x.stageIndex}>
          <a href="#" onClick={setStage.bind(null, x.stageIndex)}>{getStageTitle(x.stageIndex)}</a>
        </li>
        ), stages)
      }
    </ul>
  </li>
);

      // <li className="dropdown-header">Focus</li>
      // <li><a href="#">U.UnitUnicode <span className="badge">128,491,902</span></a></li>
      // <li className="dropdown-header">Result</li>
      // <li><a href="#">U.UnitUnicode <span className="badge">128,491,902</span></a></li>
      // <li role="separator" className="divider"></li>
const NavTypes = ({types, currentType, getType, setType}) => (
  <li role="presentation" className="dropdown">
    <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      <b>View</b>: {getType(currentType).title} <span className="caret"></span>
    </a>
    <ul className="dropdown-menu">
      { R.map(x => (
        <li key={x}>
          <a href="#" onClick={setType.bind(null, x)}>{getType(x).title} <CountBadge count={getType(x).values.length} /></a>
        </li>
        ), types)
      }
    </ul>
  </li>
);

const NavGroups = ({groups, currentType, currentGroup, getGroupTitle, setGroup}) => (
  <li role="presentation" className="dropdown">
    <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
      <b>Group By</b>: {getGroupTitle(currentGroup)}  <span className="caret"></span>
    </a>
    <ul className="dropdown-menu">
      { R.map(x => (
        <li key={currentType + '.' + x}>
          <a href="#" onClick={setGroup.bind(null, x)}>{getGroupTitle(x)}</a>
        </li>
        ), groups)
      }
    </ul>
  </li>
);

const Nav = ({getType, getGroupTitle, stages, types, groups, currentStage, currentType, currentGroup, setStage, setType, setGroup}) => {
  const getTypeTitle = typeIndex => getType(typeIndex).title
  const getStageTitle = stageIndex => getTypeTitle(stages[stageIndex].topLevelType);
  return (
    <nav className="navbar navbar-default">
      <ul className="nav nav-pills">
        <NavStages stages={stages} currentStage={currentStage} getStageTitle={getStageTitle} setStage={setStage} />
        <NavTypes types={types} currentType={currentType} getType={getType} setType={setType} />
        <NavGroups groups={groups} currentType={currentType} currentGroup={currentGroup} getGroupTitle={getGroupTitle} setGroup={setGroup} />
      </ul>
    </nav>);
};

const CountBadge = ({count}) => (<span className="badge">{count}</span>);

const Content = ({values, groupName, dataName, item, title}) => {
  const elementId = 'accordion';
  return (
    <div className="panel-group" id={elementId} role="tablist" aria-multiselectable="true">
      { R.map(x =>
        {
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
                <ul className="list-group">
                  { R.map(x => <li key={x[dataName]} className="list-group-item">{x.title}</li>, item.values) }
                </ul>
              </div>
            </div>);
        }, values)
      }
    </div>);
};

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
    const getGroupTitle = x => x === undefined ? 'None' : getType(x).title;

    const currentStageIndex = this.state.currentStage || 0;
    const currentStage = this.props.data.stages[currentStageIndex];

    const stageTypes = currentStage.allTypes;
    const currentTypeIndex = this.state.currentType && R.contains(this.state.currentType, stageTypes) ? this.state.currentType : stageTypes[0];
    const currentType = getType(currentTypeIndex);

    const typeGroups = R.concat([undefined], currentType.propertyTypes);
    const currentGroupIndex = this.state.currentGroup && R.contains(this.state.currentGroup, typeGroups) ? this.state.currentGroup : undefined;

    const groupByFunc = currentGroupIndex === undefined ? x => x.valueIndex : x => x.propertyValues[currentGroupIndex];
    const values = currentType.values.groupBy(groupByFunc, currentType.values);

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

          setStage={this.setStage.bind(this)}
          setType={this.setType.bind(this)}
          setGroup={this.setGroup.bind(this)}
        />

        <Content
        />
      </div>);
  }
}

// function renderGroup(data, currentStage) {
//   const actualCurrentStage = currentStage || 0;
//   const renderAgain = R.curry(renderGroup)(data);
//   const getType = R.curry(getTypeInfo)(data);
//   ReactDOM.render(
//     <Nav stages={data.stages} currentStage={actualCurrentStage} getType={getType} renderAgain={renderAgain} />,
//     document.getElementById('nav'));

//   // const mainId = 'main';
//   // var renderedGroups = R.map(x => (<GroupCollapse key={x.key} elementId={mainId} groupName={groupName} dataName={dataName} item={x} title={titleData[x.key].title} />), data.items);

//   // ReactDOM.render(
//   //   <div>{renderedGroups}</div>,
//   //   document.getElementById(mainId));
// }

// function doGroup(groupName) {
//   return R.compose(
//     R.last,
//     R.mapAccum((i, x) => [i + 1, { index: i, key: x.key, values: x.values } ], 0),
//     xs => R.map(x => ({ key: x, values: xs[x] }), R.keys(xs)),
//     R.groupBy(x => x[groupName])
//     );
// }
