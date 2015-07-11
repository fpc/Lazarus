-- phpMyAdmin SQL Dump
-- version 2.11.5.1
-- http://www.phpmyadmin.net
--
-- Host: localhost
-- Generatie Tijd: 16 Aug 2008 om 14:19
-- Server versie: 5.0.45
-- PHP Versie: 5.1.6

SET SQL_MODE="NO_AUTO_VALUE_ON_ZERO";

--
-- Database: `laz_testsuite`
--

-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTCPU`
--

DROP TABLE IF EXISTS `TESTCPU`;
CREATE TABLE IF NOT EXISTS `TESTCPU` (
  `TC_ID` int(11) NOT NULL auto_increment,
  `TC_NAME` varchar(10) default NULL,
  PRIMARY KEY  (`TC_ID`),
  UNIQUE KEY `TC_INAME` (`TC_NAME`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=8 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTCPU`
--

INSERT INTO `TESTCPU` (`TC_ID`, `TC_NAME`) VALUES
(5, 'x86_64'),
(4, 'arm'),
(3, 'powerpc'),
(2, 'sparc'),
(1, 'i386'),
(6, 'powerpc64'),
(7, 'All');

-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTFPCVERSION`
--

DROP TABLE IF EXISTS `TESTFPCVERSION`;
CREATE TABLE IF NOT EXISTS `TESTFPCVERSION` (
  `TFV_ID` int(11) NOT NULL auto_increment,
  `TFV_VERSION` varchar(10) default NULL,
  `TFV_RELEASEDATE` timestamp NOT NULL default CURRENT_TIMESTAMP,
  PRIMARY KEY  (`TFV_ID`),
  UNIQUE KEY `TFV_INAME` (`TFV_VERSION`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=6 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTFPCVERSION`
--

INSERT INTO `TESTFPCVERSION` (`TFV_ID`, `TFV_VERSION`, `TFV_RELEASEDATE`) VALUES
(1, '2.2.0', '2008-04-08 23:13:10'),
(2, '2.2.1', '2008-04-08 23:13:10'),
(3, '2.2.2', '2008-04-08 23:14:27'),
(4, '2.3.1', '2008-04-08 23:14:27'),
(0, 'All', '2008-04-09 23:42:58');

-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTLAZVERSION`
--

DROP TABLE IF EXISTS `TESTLAZVERSION`;
CREATE TABLE IF NOT EXISTS `TESTLAZVERSION` (
  `TLV_ID` int(11) NOT NULL auto_increment,
  `TLV_VERSION` varchar(10) default NULL,
  `TLV_RELEASEDATE` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  PRIMARY KEY  (`TLV_ID`),
  UNIQUE KEY `TLV_INAME` (`TLV_VERSION`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=6 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTLAZVERSION`
--

INSERT INTO `TESTLAZVERSION` (`TLV_ID`, `TLV_VERSION`, `TLV_RELEASEDATE`) VALUES
(1, '0.9.24', '2008-04-08 20:25:22'),
(2, '0.9.24.1', '2008-04-08 23:08:02'),
(3, '0.9.25', '2008-04-08 23:08:34'),
(0, 'All', '2008-04-09 23:36:38');

-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTOS`
--

DROP TABLE IF EXISTS `TESTOS`;
CREATE TABLE IF NOT EXISTS `TESTOS` (
  `TO_ID` int(11) NOT NULL auto_increment,
  `TO_NAME` varchar(10) default NULL,
  PRIMARY KEY  (`TO_ID`),
  UNIQUE KEY `TR_INAME` (`TO_NAME`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=16 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTOS`
--

INSERT INTO `TESTOS` (`TO_ID`, `TO_NAME`) VALUES
(1, 'linux'),
(2, 'win32'),
(3, 'go32v2'),
(4, 'os2'),
(5, 'freebsd'),
(6, 'netbsd'),
(7, 'openbsd'),
(8, 'amiga'),
(9, 'atari'),
(10, 'qnx'),
(11, 'beos'),
(12, 'sunos'),
(13, 'darwin'),
(14, 'macos'),
(0, 'All');

-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTRESULTS`
--

DROP TABLE IF EXISTS `TESTRESULTS`;
CREATE TABLE IF NOT EXISTS `TESTRESULTS` (
  `TR_ID` int(11) NOT NULL auto_increment,
  `TR_TESTRUN_FK` int(11) NOT NULL,
  `TR_TEST_FK` int(11) default NULL,
  `TR_OK` char(1) NOT NULL default '-',
  `TR_SKIP` char(1) NOT NULL default '-',
  `TR_RESULT` int(11) NOT NULL default '0',
  `TR_LOG` text,
  PRIMARY KEY  (`TR_ID`),
  KEY `I_TRTESTRUN` (`TR_TESTRUN_FK`),
  KEY `I_TRTEST` (`TR_TEST_FK`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTRESULTS`
--


-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTRUN`
--

DROP TABLE IF EXISTS `TESTRUN`;
CREATE TABLE IF NOT EXISTS `TESTRUN` (
  `TU_ID` int(11) NOT NULL auto_increment,
  `TU_DATE` timestamp NOT NULL default CURRENT_TIMESTAMP on update CURRENT_TIMESTAMP,
  `TU_CPU_FK` int(11) NOT NULL,
  `TU_OS_FK` int(11) NOT NULL,
  `TU_WS_FK` int(11) NOT NULL,
  `TU_FPC_VERSION_FK` int(11) NOT NULL,
  `TU_LAZ_VERSION_FK` int(11) NOT NULL,
  `TU_TESTCOUNT` int(11) NOT NULL,
  `TU_ERRORCOUNT` int(11) NOT NULL,
  `TU_FAILURECOUNT` int(11) NOT NULL,
  `TU_IGNORECOUNT` int(11) NOT NULL,
  `TU_SUBMITTER` varchar(40) NOT NULL,
  `TU_MACHINE` varchar(40) NOT NULL,
  `TU_COMMENT` varchar(80) NOT NULL,
  PRIMARY KEY  (`TU_ID`),
  UNIQUE KEY `TU_UNIQUE` (`TU_DATE`,`TU_CPU_FK`,`TU_OS_FK`,`TU_WS_FK`,`TU_FPC_VERSION_FK`,`TU_LAZ_VERSION_FK`),
  KEY `TU_IDATE` (`TU_DATE`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTRUN`
--


-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTS`
--

DROP TABLE IF EXISTS `TESTS`;
CREATE TABLE IF NOT EXISTS `TESTS` (
  `T_ID` int(11) NOT NULL auto_increment,
  `T_NAME` varchar(255) NOT NULL default '',
  `T_FULLNAME` varchar(255) NOT NULL default '',
  `T_CPU` varchar(20) default NULL,
  `T_OS` varchar(30) default NULL,
  `T_VERSION` varchar(10) default NULL,
  `T_ADDDATE` date NOT NULL default '0000-00-00',
  `T_GRAPH` char(1) NOT NULL default '-',
  `T_INTERACTIVE` char(1) NOT NULL default '-',
  `T_RESULT` int(11) NOT NULL default '0',
  `T_FAIL` char(1) NOT NULL default '-',
  `T_RECOMPILE` char(1) NOT NULL default '-',
  `T_NORUN` char(1) NOT NULL default '-',
  `T_NEEDLIBRARY` char(1) NOT NULL default '-',
  `T_KNOWNRUNERROR` int(11) NOT NULL default '0',
  `T_KNOWN` char(1) NOT NULL default '-',
  `T_NOTE` varchar(255) default NULL,
  `T_DESCRIPTION` text,
  `T_SOURCE` text,
  `T_OPTS` varchar(255) default NULL,
  PRIMARY KEY  (`T_ID`),
  UNIQUE KEY `TESTNAME` (`T_NAME`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1 AUTO_INCREMENT=1 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTS`
--


-- --------------------------------------------------------

--
-- Tabel structuur voor tabel `TESTWIDGETSET`
--

DROP TABLE IF EXISTS `TESTWIDGETSET`;
CREATE TABLE IF NOT EXISTS `TESTWIDGETSET` (
  `TW_ID` int(11) NOT NULL auto_increment,
  `TW_NAME` varchar(10) default NULL,
  PRIMARY KEY  (`TW_ID`),
  UNIQUE KEY `TW_INAME` (`TW_NAME`)
) ENGINE=MyISAM  DEFAULT CHARSET=latin1 AUTO_INCREMENT=10 ;

--
-- Gegevens worden uitgevoerd voor tabel `TESTWIDGETSET`
--

INSERT INTO `TESTWIDGETSET` (`TW_ID`, `TW_NAME`) VALUES
(1, 'carbon'),
(2, 'fpgui'),
(3, 'win32'),
(4, 'gtk'),
(5, 'gtk2'),
(6, 'qt'),
(0, 'All');

