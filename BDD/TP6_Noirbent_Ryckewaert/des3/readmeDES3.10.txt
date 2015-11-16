   --------------------------------
  | DES INSTALLATION (quick guide) |
   --------------------------------

================================================
Windows Binary Distribution with ACIDE GUI
================================================
- Double-click on des_acide.jar for starting the 
  ACIDE GUI

================================================
Windows Binary Distribution
================================================
- Double-click on deswin.exe for starting the Windows
  application
- Execute des.exe for starting the console application

================================================
Linux/MacOSX Binary Distribution with ACIDE GUI
================================================
- Add execute permission to file des, typically with:
  chmod +x ./bin/des
  or from the file explorer.
- Start ACIDE from a terminal with:
  java -jar des_acide.jar  

================================================
Linux/MacOSX Binary Distribution
================================================
- Add execute persmission to file des, typically with:
  chmod +x ./des
  or from the file explorer.
- Start ./des in a terminal from its installation path

================================================
Windows Source Distributions
================================================
1. Create a shortcut in the desktop for running the Prolog 
   interpreter of your choice. 
2. Modify the start directory in the "Properties" dialog box 
   of the shortcut to the installation directory for DES. 
   This allows the system to consult the needed files at startup.
3. Append the following options to the Prolog executable complete 
   filename, depending on the Prolog interpreter you use:
   (a) SICStus Prolog: -l des.pl
   (b) SWI Prolog: -g "ensure_loaded(des)" (remove --win_app if present)
Another alternative is to write a batch file similar to the 
script file described just in the above section.
    
================================================
Linux/MacOSX Source Distributions
================================================
You can write a script for starting DES according to the 
selected Prolog interpreter, as follows:
(a) SICStus Prolog: 
    $SICSTUS -l des.pl 
    Provided that $SICSTUS is the variable which holds 
    the absolute filename of the SICStus Prolog executable.
(b) SWI Prolog: 
    $SWI -g "ensure_loaded(des)"
    Provided that $SWI is the variable which holds the 
    absolute filename of the SWI Prolog executable.

================================================
More Information: 
================================================

- See User Manual
  'Documentation' entry in 
  http://www.fdi.ucm.es/profesor/fernan/des/html/download.html
- http://des.sourceforge.net


Version 3.10 of DES (released on January, 21st, 2015)
 
* Enhancements:
  o Tracing and debugging external relations (via ODBC) is allowed
  o SQL debugging with development mode enabled shows the logic program every time a clause is added
  o The PDG and stratification are also built for external SQL relations when opening an ODBC connection
  o The SQL text which defines an external view is listed for DB2, MySQL, Oracle, and PostgreSQL whenever it is recognized by the DES SQL dialect (other DBMS's might work as well, though not tested)
  o An external view for which there is no a mate Datalog predicate is tagged as an extensional predicate so that only one fixpoint iteration is needed to solve it
  o Extensional database optimization also applies to external relations
  o Incremental building of the PDG which improves performance a bit
  o The equivalent SQL statement to a given RA expression can be inspected by enabling SQL listings with the command /show_sql on (The equivalent Datalog rules were possible to show already with the command /show_compilations on)
  o Simplified compilations from RA to SQL
  o Relaxed requirement for SQL identifiers: SQL keywords can be used as identifiers for tables, views, and attributes. In order to disambiguate, it might be necessary to enclose the identifier between SQL delimiters
  o A relation with a name which coincide with the name of a Datalog metapredicate with the same arity is rejected
  o Constants can include escaped single quotes
  o Upgraded DDL info messages for external databases
  o Parameterized batch processing: Batch files can contain references to input parameters ($parv1$, $parv2$, ... )
  o Coloured printed and online manual
  o Updated colors for the console in ACIDE
  o New commands:
    - /rdg Display the current relation dependency graph, i.e., the PDG restricted to show only nodes with type information (tables and views) . TAPI enabled
    - /rdg RelName Display the current relation dependency graph restricted to the first relation found with name RelName. TAPI enabled
    - /rdg RelName/Arity Display the current relation dependency graph restricted to the relation with name RelName and Arity. TAPI enabled
    - /refresh_db Refresh local metadata from the current database (either the local, deductive database or an external DB), clear the cache, and recompute the PDG and strata. TAPI enabled
    - /repeat Number Input Repeat Input as many times as Number
    - /set_default_parameter Index Value Set the default value for the i-th parameter (denoted by the number Index) to Value

* Changes:
  o System predicates resulting from compilations (starting with $) only appear in development mode (this applies to consulting the current PDG and strata, and tracing Datalog and local SQL queries)
  o Nodes in the PDG include DDB tables
  o Infix comparisons, built-ins, and MS Access system tables are no longer part of the PDG
  o PDG arcs are ordered by nodes (previously, first were the positive arcs, then the negative arcs)
  o Warning about undefined predicates are only issued when either inserting the offending rule or recomputing the whole PDG
  o System autorenamings are not shown in displayed SQL statements
  o Built-ins in SQL expressions are capitalized in listings
  o Displayed result sets in SQL debugging are ordered
  o Oracle connections are restricted to user tables and views, therefore avoiding to retrieve dozens of system relations
  o All identifiers in system messages are enclosed between single quotes
  o The argument of the command /process must be enclosed between double quotes (") if it contains blanks
  o Calls to recursive relations are not unfolded (both in the translations from SQL and unfolding user rules when /unfold on is submitted)
  o Deprecated: SQL DROP TABLE Tablenames

* Fixed bugs:
  o The SQL Debugger tried to slice tables when they were not trusted
  o References to attribute names in the GROUP BY clause were not always resolved
  o SQL listings including the DIVISION operator printed its internal representation
  o Some metadata for views were retrieved as they were tables for Oracle databases
  o Some ODBC diag exceptions were not formatted in the display
  o Persistence in DB2 connections was faulty for SWI-Prolog distros
  o Listings involving the top and sort RA operators failed
  o The SQL ON clause required a blank after it
  o Printing expressions in SQL ORDER BY failed in listings
  o When adding a rule for a new predicate calling a restricted one, the PDG did not include the negative dependency between them
