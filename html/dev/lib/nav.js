import React from 'react';
import { Navbar, NavBrand, NavItem, Nav as BNav } from 'react-bootstrap';

export const Nav = ({ title, workListUrl, typeListUrl, licenseUrl }) => {
  return (
    <Navbar fluid>
      <NavBrand>{title}</NavBrand>
      <BNav right>
        <NavItem eventKey={0} href={workListUrl}>Works</NavItem>
        <NavItem eventKey={1} href={typeListUrl}>Types</NavItem>
        <NavItem eventKey={2} href={licenseUrl}>License</NavItem>
      </BNav>
    </Navbar>
  );
};
