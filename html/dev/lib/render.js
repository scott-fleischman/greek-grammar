import R from 'ramda';
import React from 'react';
import FixedDataTable from 'fixed-data-table';
import { OverlayTrigger, Button, Popover, Navbar, NavBrand, Nav, NavItem, NavDropdown, MenuItem } from 'react-bootstrap';

const Table = FixedDataTable.Table;
const Column = FixedDataTable.Column;

// const NavLink = ({item, getTitle, setItem, count}) => (
//   <a href="#" onClick={setItem.bind(null, item)}>{getTitle(item)} {count ? <CountBadge count={count} /> : undefined}</a>
// );

// const NavStages = ({stages, currentStage, getStageTitle, setStage}) => (
//   <li role="presentation" className="dropdown">
//     <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
//       <b>Stage</b>: {getStageTitle(currentStage)} <span className="caret"></span>
//     </a>
//     <ul className="dropdown-menu">
//       { R.map(x => (
//         <li key={x.stageIndex}>
//           <NavLink item={x.stageIndex} getTitle={getStageTitle} setItem={setStage} />
//         </li>
//         ), stages)
//       }
//     </ul>
//   </li>
// );

// const NavTypes = ({types, focusSource, focusResult, currentType, getType, setType}) => {
//   const getTypeTitle = x => getType(x).typeTitle;
//   const getValueCount = x => getType(x).values.length;
//   const LocalNavLink = ({index}) => (<NavLink item={index} getTitle={getTypeTitle} setItem={setType} count={getValueCount(index)} />);
//   return (
//     <li role="presentation" className="dropdown">
//       <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
//         <b>View</b>: {getTypeTitle(currentType)} <span className="caret"></span>
//       </a>
//       <ul className="dropdown-menu">
//         { R.flatten(
//             [ !R.isNil(focusResult) ?
//               ( [ <li key="focusResult" className="dropdown-header">Focus Result</li>
//                 , <li key={focusResult}><LocalNavLink index={focusResult} /></li>
//                 ]
//               ) :
//               [],
//             , !R.isNil(focusSource) && focusSource !== focusResult ?
//               ( [ <li key="focusSource" className="dropdown-header">Focus Source</li>
//                 , <li key={focusSource}><LocalNavLink index={focusSource} /></li>
//                 ]
//               ) :
//               [],
//             , !R.isNil(oneOf(focusResult, focusSource)) ?
//               ( [ <li key="separator" role="separator" className="divider"></li> ] ) :
//               [],
//             , R.map(x => (<li key={x}><LocalNavLink index={x} /></li>), types)
//             ]
//           )
//         }
//       </ul>
//     </li>
//   );
// };

// const NavGroups = ({groups, currentType, currentGroup, getGroupTitle, setGroup}) => (
//   <li role="presentation" className="dropdown">
//     <a className="dropdown-toggle" data-toggle="dropdown" href="#" role="button" aria-haspopup="true" aria-expanded="false">
//       <b>Group By</b>: {getGroupTitle(currentGroup)}  <span className="caret"></span>
//     </a>
//     <ul className="dropdown-menu">
//       { R.map(x => (
//         <li key={currentType + '.' + x}>
//           <NavLink item={x} getTitle={getGroupTitle} setItem={setGroup} />
//         </li>
//         ), groups)
//       }
//     </ul>
//   </li>
// );

// const Nav = ({getType, getGroupTitle, stages, types, groups, currentStage, currentType, currentGroup, focusResult, focusSource, setStage, setType, setGroup}) => {
//   const getTypeTitle = typeIndex => getType(typeIndex).typeTitle;
//   const getStageTitle = stageIndex => getTypeTitle(stages[stageIndex].topLevelType);
//   return (
//     <nav className="navbar navbar-default">
//       <ul className="nav nav-pills">
//         <NavStages
//           stages={stages}
//           currentStage={currentStage}
//           getStageTitle={getStageTitle}
//           setStage={setStage} />

//         <NavTypes
//           types={types}
//           currentType={currentType}
//           focusResult={focusResult}
//           focusSource={focusSource}
//           getType={getType}
//           setType={setType} />

//         <NavGroups
//           groups={groups}
//           currentType={currentType}
//           currentGroup={currentGroup}
//           getGroupTitle={getGroupTitle}
//           setGroup={setGroup} />
//       </ul>
//     </nav>);
// };

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

const Word = ({id, text, wordProps, propertyNames, summaryProperties}) => {
  const popoverList = R.map (x => (<div key={x}><strong>{propertyNames[x]}</strong>: {wordProps[x]}</div>)) (summaryProperties);
  const popover = (
    <Popover id={id} title="Properties" style={{maxWidth: '100%'}}>
      {popoverList}
    </Popover>
  );
  return (
    <OverlayTrigger trigger={['hover', 'focus']} placement="bottom" overlay={popover}>
      <span><a href="#">{text}</a> </span>
    </OverlayTrigger>
  );
}

const WordGroup = ({id, words}) => {
  const wordElements = R.map (x => (<Word key={x.key} {...x} />)) (words);
  return (
    <div style={{margin: '0 0 1em 0'}}>
      {wordElements}
    </div>
  );
}

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

class FullSizeGrid extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      tableWidth: 300,
      tableHeight: 300,
    };
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
}

const OurNavbar = ({currentWork, works, onSelect}) => {
  const myWorks = R.addIndex(R.map) ((x, i) => (<MenuItem key={i} eventKey={i}>{x.workSource} &mdash; {x.workTitle}</MenuItem>)) (works);

  return (
    <Navbar>
      <NavBrand>Greek Grammar</NavBrand>
      <Nav onSelect={onSelect}>
        <NavDropdown title={'Work: ' + works[currentWork].workTitle}>
          {myWorks}
        </NavDropdown>
      </Nav>
    </Navbar>
  );
};

export class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      currentWork: 23,
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
  onNavSelect(event, selectedKey) {
    this.setState({
      currentWork: selectedKey
    });
  }

  render() {
    // const getType = x => this.props.data.types.get(x);
    // const getGroupTitle = x => R.isNil(x) ? noneTitle : getType(x).typeTitle;

    // const currentStageIndex = oneOf(this.state.currentStage, 0);
    // const currentStage = this.props.data.stages[currentStageIndex];

    // const focusResult = currentStage.focusResultType;
    // const focusSource = currentStage.focusSourceType;
    // const stageTypes = R.filter(x => x !== focusResult && x !== focusSource, currentStage.allTypes);
    // const currentTypeIndex = this.state.currentType && R.contains(this.state.currentType, stageTypes) ?
    //   this.state.currentType :
    //   oneOf(focusResult, focusSource, stageTypes[0]);
    // const currentType = getType(currentTypeIndex);

    // const typeGroups = R.concat([undefined], currentType.propertyTypes);
    // const currentGroupIndex = this.state.currentGroup && R.contains(this.state.currentGroup, typeGroups) ? this.state.currentGroup : undefined;

    // const contentInfo = R.isNil(currentGroupIndex) ?
    //   getPropertyContentInfo(getType, currentTypeIndex, currentType) :
    //   getGroupedContentInfo(getType, currentGroupIndex, currentType.values);

    const data = this.props.data;
    // const myRowCount = data.stage0.instanceValues.length;
    // const myRowGetter = x => {
    //   const getPropertyName = i => data.stage0.instanceProperties[i];
    //   const getPropertyValue = (n, v) => data.index.properties.get(n)[v];
    //   const getTextValue = (v, i) => {
    //     const name = getPropertyName(i);
    //     const value = getPropertyValue(name, v);
    //     return value;
    //   };
    //   return R.addIndex(R.map) (getTextValue) (data.stage0.instanceValues[x]);
    // };
    // const columns = R.addIndex(R.map)((x, i) => (<Column label={x} width={200} flexGrow={1} dataKey={i} key={x} />), data.stage0.instanceProperties);

    const currentWork = this.props.data.works[this.state.currentWork];
    const currentWordGroup = currentWork.workWordGroups[0];
    const currentWordGroupWords = currentWordGroup.wordGroupWords;

    const convertWords = (groupKey, propertyNames, summaryProperties) => (R.addIndex(R.map)
      ((x, i) => ({
        key: groupKey + '.' + i,
        id: 'word.' + groupKey + '.' + i,
        text: x.t,
        wordProps: x.p,
        propertyNames: propertyNames,
        summaryProperties: summaryProperties,
      })));

    const wordGroups = R.addIndex(R.map)
      ((wordIndexes, groupIndex) => {
        const groupKey = this.state.currentWork + '.' + groupIndex;
        const wordInfos = R.map(x => currentWork.workWords[x]) (wordIndexes);
        const words = convertWords(groupKey, currentWork.workWordPropertyNames, currentWork.workWordSummaryProperties) (wordInfos);
        return (<WordGroup key={groupKey} words={words} />);
      })
      (currentWordGroupWords);

    return (
      <div>
        <OurNavbar
          currentWork={this.state.currentWork}
          works={this.props.data.works}
          onSelect={this.onNavSelect.bind(this)}
          />
        <div style={{margin: '0 1em 200px 1em'}}>
          {wordGroups}
        </div>
      </div>);
  }
}
        // <Table
        //   rowHeight={50}
        //   rowGetter={myRowGetter}
        //   rowsCount={myRowCount}
        //   width={this.state.tableWidth}
        //   height={this.state.tableHeight}
        //   headerHeight={50}>
        //   {columns}
        // </Table>

        // <Nav
        //   getType={getType}
        //   getGroupTitle={getGroupTitle}

        //   stages={this.props.data.stages}
        //   types={stageTypes}
        //   groups={typeGroups}

        //   currentStage={currentStageIndex}
        //   currentType={currentTypeIndex}
        //   currentGroup={currentGroupIndex}

        //   focusSource={focusSource}
        //   focusResult={focusResult}

        //   setStage={this.setStage.bind(this)}
        //   setType={this.setType.bind(this)}
        //   setGroup={this.setGroup.bind(this)}
        // />
