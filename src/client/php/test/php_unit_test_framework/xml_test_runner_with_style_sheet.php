<?php
//////////////////////////////////////////////////////////
///
/// $Author: edheal $
/// $Date: 2011-04-05 17:57:18 +0100 (Tue, 05 Apr 2011) $
/// $Id: xml_test_runner_with_style_sheet.php 12 2011-04-05 16:57:18Z edheal $
///
/// \file
/// \brief Used to apply XSLT style sheet to XML output.
///
/// \details
///
/// The abstract class is to be subclasses to enable a
/// XSLT style sheet to generate the output from the XML
/// generated by XMLTestRunnerClass.
///
/// Two examples of this are present
/// - XHTMLTestRunner: Generates XHTML output.
/// - TextTestRunner: Generates text (ASCII) Output.
///
/// \section License
///
/// A PHP Unit testing framework
///
/// Copyright (C) 2011 Ed Heal (ed.heal@yahoo.co.uk)
///
/// This program is free software: you can redistribute it and/or modify
/// it under the terms of the GNU General Public License as published by
/// the Free Software Foundation, either version 3 of the License, or
/// (at your option) any later version.
///
/// This program is distributed in the hope that it will be useful,
/// but WITHOUT ANY WARRANTY; without even the implied warranty of
/// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
/// GNU General Public License for more details.
///
/// You should have received a copy of the GNU General Public License
/// along with this program.  If not, see <http://www.gnu.org/licenses/>.
///
//////////////////////////////////////////////////////////

require_once 'xml_test_runner.php';

//////////////////////////////////////////////////////////
/// \brief This test runner generates the report in XML
///        and apply a style sheet to it.
///
/// The class uses XMLTestRunner to generate a report and
/// then transforms this report using XSLT.
//////////////////////////////////////////////////////////
abstract class XMLTestRunnerWithStyleSheet extends XMLTestRunner
{
	protected $styleSheet; ///< Style sheet to use

	//////////////////////////////////////////////////////////
	/// \brief Initializes the first part of the style sheet.
	///
	/// This constructs the first two lines of the style sheet
	/// which is common to all XSLT style sheets.
	/// i.e.
	/// <pre>
	/// <?xml version="1.0" encoding="US-ASCII"?&gt;
	/// <xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	/// </pre>
	///
	//////////////////////////////////////////////////////////
	public function __construct()
	{
		$this->styleSheet = '<?xml version="1.0" encoding="US-ASCII"?' .
                         ">\n<xsl:stylesheet version=\"1.0\"\n" .
                         "                xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">\n";
	}

	//////////////////////////////////////////////////////////
	/// \brief Applies the style sheet to a generated XML report.
	///
	/// Generates the report in XML and applies the style sheet
	//////////////////////////////////////////////////////////
	public function Report()
	{
		$xsl = new DOMDocument;
		if (!$xsl->loadXML($this->styleSheet))
		{
			throw new Exception("Unable to parse sylesheet:\n" . $this->styleSheet);
		}
		$xml = new DOMDocument;
		$report = parent::Report();

		//$xml->preserveWhiteSpace = false;
		if (!$xml->loadXML($report))
		{
			throw new Exception("Unable to parse XML:\n" . $report);
		}

		$proc = new XSLTProcessor;
		$proc->importStyleSheet($xsl);

		return $proc->transformToXML($xml);
	}
}
?>