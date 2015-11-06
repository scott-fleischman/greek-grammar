import React from 'react';
import * as BS from 'react-bootstrap';

export const Nav = ({ title, workListUrl, typeListUrl }) => {
  console.log(workListUrl, typeListUrl);
  return (
    <BS.Navbar fluid>
      <BS.NavBrand>{title}</BS.NavBrand>
      <BS.Nav right>
        <BS.NavItem eventKey={0} href={workListUrl}>Works</BS.NavItem>
        <BS.NavItem eventKey={1} href={typeListUrl}>Types</BS.NavItem>
      </BS.Nav>
    </BS.Navbar>
  );
};
