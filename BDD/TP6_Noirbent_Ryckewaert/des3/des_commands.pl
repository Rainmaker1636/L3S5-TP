/*********************************************************/
/*                                                       */
/* DES: Datalog Educational System v.3.10                */
/*                                                       */
/*    Commands                                           */
/*                                                       */
/*                                                       */
/*                                                       */
/*                    Fernando Saenz-Perez (c) 2004-2015 */
/*                                         DISIA GPD UCM */
/*             Please send comments, questions, etc. to: */
/*                                     fernan@sip.ucm.es */
/*                                Visit the Web site at: */
/*                           http://des.sourceforge.net/ */
/*                                                       */
/* This file is part of DES.                             */
/*                                                       */
/* DES is free software: you can redistribute it and/or  */
/* modify it under the terms of the GNU Lesser General   */
/* Public License as published by the Free Software      */
/* Foundation, either version 3 of the License, or (at   */
/* your option) any later version.                       */
/*                                                       */
/* DES is distributed in the hope that it will be useful,*/
/* but WITHOUT ANY WARRANTY; without even the implied    */
/* warranty of MERCHANTABILITY or FITNESS FOR A          */
/* PARTICULAR PURPOSE. See the GNU Lesser General Public */
/* License for more details.                             */
/*                                                       */
/* You should have received a copy of the GNU Lesser     */
/* General Public License and GNU General Public License */
/* along with this program. If not, see:                 */
/*                                                       */
/*            http://www.gnu.org/licenses/               */
/*********************************************************/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PARSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

is_command(CInput) :-
  command_begin(CInput,_).
  
% is_tapi_command(Cs) :-
%   is_tapi_command(Cs,_).

% is_tapi_command -->
%   command_begin,
%   my_blanks_star, 
%   my_kw("TAPI"),
%   my_blanks.

command_begin -->
  my_blanks_star,
  "/*",
  {!,
   fail}.
command_begin -->
  my_blanks_star,
  "/".
  
my_command_input(Input,Command) :-
  my_command_input(Command,Input,_).
  
my_command_input(reconsult) -->
  command_begin,
  my_blanks_star, 
  "[+",
  !.
my_command_input(consult) -->
  command_begin,
  my_blanks_star, 
  "[",
  !.
my_command_input(repeat(_N)) -->
  command_begin,
  my_blanks_star, 
  my_kw("REPEAT"),
  !.
my_command_input(Command) -->
  command_begin,
  my_blanks_star, 
  my_command(Command),
  my_blanks_star.

  
    
parse_command(assert,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("ASSERT"),
  " ",
  !,
  parse_rule(Rule,[],NVs),
  my_blanks_star.
parse_command(retract,[Rule],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("RETRACT"),
  " ",
  !,
  my_blanks_star,
  my_rule(Rule,[],NVs),
  my_blanks_star.
parse_command(des,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("DES"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_command(tapi,[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("TAPI"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_command(Process,[File|Params],[]) -->
  command_begin,
  my_blanks_star, 
  my_process_command(Process),
  my_blanks,
  !,
  my_argument(File),
  my_params(Params).
parse_command(repeat(N),[Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("REPEAT"),
  my_blanks,
  !,
  my_positive_integer(N),
  my_blanks,
  my_chars(Cs),
  {name(Input,Cs)},
  my_blanks_star.
parse_command(set_default_parameter,[I,Value],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SET_DEFAULT_PARAMETER"),
  my_blanks,
  !,
  my_positive_integer(I),
  my_blanks,
  my_chars(Value).
parse_command(shell,[Command],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SHELL"),
  my_blanks,
  !,
  my_chars(Cs),
  {name(Command,Cs)},
  my_blanks_star.
parse_command(Log,[append,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
  my_blanks,
  my_kw("APPEND"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(Log,[write,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks,
  my_kw("WRITE"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(Log,[write,normal,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(Log,[],[]) -->
  command_begin,
  my_blanks_star, 
  my_log_command(Log),
%  my_kw("LOG"),
  my_blanks_star,
  !.
parse_command(write_to_file,[File,Input],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("WRITE_TO_FILE"),
  !,
  my_blanks,
  my_chars_but_blanks(Cs),
  {name(File,Cs)},
  my_blank,
  my_chars(StrInput),
  {name(Input,StrInput)}.
parse_command(save_ddb,[force,File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_DDB"),
  my_blanks,
  my_kw("FORCE"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(save_ddb,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("SAVE_DDB"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(restore_ddb,[File],[]) -->
  command_begin,
  my_blanks_star, 
  my_kw("RESTORE_DDB"),
  my_blanks,
  my_charsbutcomma(Cs),
  {name(File,Cs)},
  my_blanks_star,
  !.
parse_command(reconsult,Files,[]) -->
  command_begin,
  my_blanks_star, 
  "[+",
  my_blanks_star,
  my_arguments(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
parse_command(consult,Files,[]) -->
  command_begin,
  my_blanks_star,
  "[",
  my_blanks_star,
  my_arguments(Files),
  my_blanks_star,
  "]",
  my_blanks_star,
  !.
parse_command(drop_ic,[Constraint],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DROP_IC"),
  parse_datalog_constraint(Constraint,NVs),
  !.
parse_command(drop_assertion,[Assertion],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DROP_ASSERTION"),
  my_blanks_star,
  parse_datalog_assertion(Assertion,NVs),
  !.
% parse_command(datalog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(datalog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
% parse_command(prolog,[Goal],NVs) -->
%   command_begin,
%   my_blanks_star,
%   my_command(prolog),
%   " ",
%   !,
%   my_blanks_star,
%   my_body(Goal,[],NVs),
%   my_blanks_star. 
parse_command(dependent_relations,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEPENDENT_RELATIONS"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(dependent_relations,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEPENDENT_RELATIONS"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(referenced_relations,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("REFERENCED_RELATIONS"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(referenced_relations,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("REFERENCED_RELATIONS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(spy,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("SPY"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(nospy,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("NOSPY"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(system,[Goal],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("SYSTEM"),
  my_blanks,
  my_body(Goal,[],_NVs),
  my_blanks_star,
  !.
parse_command(abolish,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("ABOLISH"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(list_schema,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_SCHEMA"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(dbschema,[C:N],[]) -->
  {DBSchema = "DBSCHEMA" ; DBSchema = "DB_SCHEMA"},
  command_begin,
  my_blanks_star,
  my_kw(DBSchema),
  my_blanks,
  my_sql_user_identifier(C),
  my_blanks_star,
  ":",
  my_blanks_star,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(dbschema,[N],[]) -->
  {DBSchema = "DBSCHEMA" ; DBSchema = "DB_SCHEMA"},
  command_begin,
  my_blanks_star,
  my_kw(DBSchema),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(dbschema,[],[]) -->
  {DBSchema = "DBSCHEMA" ; DBSchema = "DB_SCHEMA"},
  command_begin,
  my_blanks_star,
  my_kw(DBSchema),
  my_blanks_star,
  !.
parse_command(relation_schema,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_SCHEMA"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(relation_exists,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_EXISTS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(is_empty,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RELATION_EXISTS"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(list_table_constraints,[TableName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_TABLE_CONSTRAINTS"),
  my_blanks,
  my_sql_user_identifier(TableName),
  my_blanks_star.
parse_command(list_et,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_ET"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(list_modes,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_MODES"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(list_modes,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_MODES"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(listing,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_rule(R,[],_NVs),
  my_blanks_star.
parse_command(listing,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(listing,[H],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_head(H,[],_NVs),
  my_blanks_star,
  !.
parse_command(listing,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(listing_asserted,[R],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_rule(R,[],_NVs),
  my_blanks_star.
parse_command(listing_asserted,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(listing_asserted,[H],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_head(H,[],_NVs),
  my_blanks_star,
  !.
parse_command(listing_asserted,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LISTING_ASSERTED"),
  my_blanks,
  my_sql_user_identifier(N),
  my_blanks_star,
  !.
parse_command(list_sources,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("LIST_SOURCES"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(pdg,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("PDG"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(pdg,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("PDG"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(rdg,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RDG"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(rdg,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RDG"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(strata,[N/A],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("STRATA"),
  my_blanks,
  my_pattern(N/A),
  my_blanks_star,
  !.
parse_command(strata,[N],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("STRATA"),
  my_blanks,
  my_user_identifier(N),
  my_blanks_star,
  !.
parse_command(retractall,[Head],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("RETRACTALL"),
  " ",
  !,
  my_blanks_star,
  my_head(Head,[],_NVs),
  my_blanks_star.
parse_command(open_db,[Connection|Options],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("OPEN_DB"),
  my_blanks,
  !,
  my_symbol(Connection),
  my_blanks_star,
  my_atoms_star(Options,[],_),
  my_blanks_star.
parse_command(close_db,[Connection],_NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("CLOSE_DB"),
  my_blanks,
  !,
  my_symbol(Connection),
  my_blanks_star.
% parse_command(debug_dl,[N/A,File],[]) -->
%   command_begin,
%   my_blanks_star,
%   my_kw("DEBUG_DL"),
%   my_blanks,
%   my_pattern(N/A),
%   my_blanks,
%   my_charsbutcomma(Fs),
%   {name(File,Fs)},
%   my_blanks_star,
%   !.
parse_command(debug_datalog,[Goal,Level],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DATALOG"),
  my_blanks,
  my_literal(Goal,[],NVs),
  my_blanks,
  my_symbol(Level),
  my_blanks_star,
  !.
parse_command(debug_datalog,[Goal],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_DATALOG"),
  my_blanks,
  !,
  my_literal(Goal,[],NVs),
  my_blanks_star.
parse_command(debug_sql,[ViewName,Opt1,Opt2,Opt3],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_atom(Opt1,[],[]),
  my_blanks,
  my_atom(Opt2,[],[]),
  my_blanks,
  my_atom(Opt3,[],[]),
  my_blanks_star,
  !.
parse_command(debug_sql,[ViewName,Opt1,Opt2],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_atom(Opt1,[],[]),
  my_blanks,
  my_atom(Opt2,[],[]),
  my_blanks_star,
  !.
parse_command(debug_sql,[ViewName,Opt],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_atom(Opt,[],[]),
  my_blanks_star,
  !.
parse_command(debug_sql,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("DEBUG_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks_star,
  !.
parse_command(trace_sql,[ViewName,Ordering],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol(Ordering),
  my_blanks_star,
  !.
parse_command(trace_sql,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_SQL"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks_star,
  !.
parse_command(trace_datalog,[Query,Ordering],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_DATALOG"),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks,
  my_symbol(Ordering),
  my_blanks_star,
  !.
parse_command(trace_datalog,[Query],NVs) -->
  command_begin,
  my_blanks_star,
  my_kw("TRACE_DATALOG"),
  my_blanks,
  my_literal(Query,[],NVs),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName,Opt1,Opt2],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol(Opt1),
  my_blanks,
  my_symbol(Opt2),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName,Opt],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks,
  my_symbol(Opt),
  my_blanks_star,
  !.
parse_command(test_case,[ViewName],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TEST_CASE"),
  my_blanks,
  my_sql_user_identifier(ViewName),
  my_blanks_star,
  !.
parse_command(tc_size,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TC_SIZE"),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_command(tc_domain,[Min,Max],[]) -->
  command_begin,
  my_blanks_star,
  my_kw("TC_DOMAIN"),
  my_blanks,
  !,
  my_integer(Min),
  my_blanks_star,
  my_integer(Max),
  my_blanks_star.
parse_command(Command,[],[]) -->
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks_star
  %,!
  .
parse_command(Command,Arguments,[]) -->
  %WARNING: This is needed to avoid the cut in the clause above. 
  {Arguments\==[]},
  command_begin,
  my_blanks_star,
  my_command(Command),
  my_blanks,
  my_arguments(Arguments),
  my_blanks_star.

  
my_command(Command) -->
  my_chars_but_blank(Cs),
  {to_lowercase_char_list(Cs,LCs),
   name(Command,LCs)}.
   
my_log_command(log) -->
  my_kw("LOG").
my_log_command(logiql_log) -->
  my_kw("LOGIQL_LOG").

my_process_command(Process) -->
  my_kw("PROCESS"),
  !,
  {Process=p ; Process=process}.
my_process_command(Process) -->
  my_kw("P"),
  {Process=p ; Process=process}.

my_argument(File) -->
  my_str_argument(StrFile),
  {name(File,StrFile)}.
  
my_str_argument(File) -->
  """",
  my_chars_but_double_quotes(File),
  """",
  !.
my_str_argument(File) -->
  my_chars_but_blank(File).
  
my_chars_but_double_quotes([C|Cs]) -->
  [C],
  {""""\==[C]},
  my_chars_but_double_quotes(Cs).
my_chars_but_double_quotes([]) -->
  [].
  
my_params([P|Ps]) -->
  my_blanks,
  my_str_argument(P),
  {P\==""},
  my_params(Ps).
my_params([]) -->
  my_blanks_star,
  [].

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

process_command(SCommand,yes) :- 
  lang_interpreter_cmd(Language,SCommand,CInput),
%  \+ (my_blanks_star(CInput,[])),
  CInput\==[],
  !,
  nl_compact_log,
  atom_concat('process_',Language,F),
  P=..[F,CInput],
  (call(P)
   ->
    nl_compact_log
   ;
    process_command_error).
process_command(SCommand,Continue) :- 
  my_command_input(SCommand,compact_listings),
  !,
%   (parse_command(Command,Arguments,NVs,SCommand,CInput),
%    my_blanks_star(CInput,[]),
  (parse_command(Command,Arguments,NVs,SCommand,[]),
   !,
   store_elapsed_time(parsing),
   compact_listings(CL),
   (((CL==off, Arguments==[off]);(CL==off, Arguments==[]);(CL==on, Arguments==[off])) -> nl_log ; true),
   processC(Command,Arguments,NVs,Continue),
   (((CL==off, Arguments==[off]);(CL==off, Arguments==[]);(CL==on, Arguments==[off])) -> nl_log ; true)
  ; 
   process_command_error
  ).
process_command(SCommand,Continue) :- 
  my_command_input(SCommand,Command),
  !,
  nl_tapi_log(Command),
%   ((parse_command(Command,Arguments,NVs,SCommand,CInput),
%    my_blanks_star(CInput,[])) -> 
  (parse_command(Command,Arguments,NVs,SCommand,[])
   -> 
    store_elapsed_time(parsing),
    processC(Command,Arguments,NVs,Continue),
    nl_tapi_log(Command)
   ; 
    process_command_error
  ).

process_command_error :-
  write_error_log(['Syntax error in command and/or its argument. Type /help for help on commands']),
  nl_compact_log.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCESSING individual commands

processC(assert,[T],NVs,yes) :-
  !, 
  assert_rule((T,NVs),[],datalog,[],CRNVs,_ODLIds,_Unsafe,Error),
  (Error==true -> 
    true
    ;
    abolishET, 
    update_stratification_add_ruleNVs(CRNVs),
    write_info_verb_log(['Rule asserted.'])
  ).
processC(retract,[R],_NVs,yes) :-
  !, 
  retract_source_rule(R,_Error).
processC(retractall,[H],_NVs,yes) :-
  !, 
  get_filtered_source_dlrules(head,H,[],SDLs),
  (SDLs==[] -> 
    write_warning_log(['Nothing retracted.'])
   ;
    retract_source_dlrule_list(SDLs,RSDLs,RODLs,_Error),
    (RSDLs\==[] ->
      abolishET,
      compute_stratification
     ;
      true
    ),
    display_tuples_and_nbr_info(RSDLs,RODLs)
  ).
processC(fail,[],_NVs,yes) :-
  !, 
  fail.
processC(des_sql_solving,[],_NVs,yes) :-
  !, 
  des_sql_solving(Switch),
  write_info_log(['Forcing DES solving for external DBMS is ', Switch, '.']).
processC(des_sql_solving,[Switch],_NVs,yes) :-
  process_set_binary_flag(des_sql_solving,'Forcing DES solving for external DBMS is',Switch).
processC(des,[Input],_NVs,yes) :-
  !,
  save_state,
  set_flag(des_sql_solving,on),
  %set_flag(compact_listings,on),
  name(Input,InputStr),
  process_input(InputStr,_Continue,no_nl),
  restore_state.
%% BEGIN TAPI commands and statements
processC(tapi,[Input],_NVs,yes) :-
  !,
  save_state,
  set_flag(tapi,on),
  set_flag(compact_listings,on),
  set_flag(language,datalog),
  name(Input,InputStr),
  process_input(InputStr,_Continue,no_nl),
  restore_state.
processC(test_tapi,[],_NVs,yes) :-
  !,
  push_flag(tapi,on,CurrentValue),
  write_tapi_success,
  pop_flag(tapi,CurrentValue).
%% END TAPI commands and statements
processC(write,[Input],_NVs,yes) :-
  !,
  name(Input,StrInput),
  instance_system_vars(TInput,StrInput,[]),
  write_string_log(TInput).
processC(writeln,[Input],NVs,yes) :-
  !,
  processC(write,[Input],NVs,yes),
  nl_log.
processC(write_to_file,[File,Input],_NVs,yes) :-
  !,
  my_absolute_filename(File,AFN),
  open(AFN,append,S),
  name(Input,StrInput),
  instance_system_vars(TInput,StrInput,[]),
  name(T,TInput),
  write(S,T),
  flush_output(S),
  close(S).
processC(writeln_to_file,[File],_NVs,yes) :-
  !,
  my_absolute_filename(File,AFN),
  open(AFN,append,S),
  nl(S),
  flush_output(S),
  close(S).
processC(sql_left_delimiter,[],_NVs,yes) :-
  !,
  current_db(_,DBMS),
  my_sql_left_quotation_mark(LQstr,DBMS),
  write_string_log(LQstr),
  nl_log.
processC(sql_right_delimiter,[],_NVs,yes) :-
  !,
  current_db(_,DBMS),
  my_sql_right_quotation_mark(RQstr,DBMS),
  write_string_log(RQstr),
  nl_log.

processC(Quit,[],_NVs,no) :- 
  (Quit=q; Quit=quit; Quit=e; Quit=exit; Quit=halt), 
  !, 
  (my_log(_F,_S) -> processC(nolog,[],_,no) ; true),
  close_dbs,
  halt.
processC(Terminate,[],_NVs,no) :- 
  (Terminate=terminate ; Terminate=t), 
  !.
processC(debug,[],_NVs,yes) :- 
  !,
  debug.
processC(Spy,[Predicate],_NVs,yes) :- 
  Spy=(spy), 
  !,
  my_spy(Predicate).
processC(NoSpyAll,[],_NVs,yes) :- 
  NoSpyAll=nospyall, 
  !,
  my_nospyall.
processC(NoSpy,[Predicate],_NVs,yes) :- 
  NoSpy=nospy, 
  !,
  my_nospy(Predicate).
processC(system,[Goal],_NVs,yes) :- 
  !, 
  (call(Goal) ->
    true
   ;
    write_log_list(['no',nl])
  ).
processC(Shell,[C],_NVs,yes) :- 
  (Shell=shell; Shell=s), 
  !, 
  my_shell(C,sync,S),
  (S=0 -> 
    write_info_verb_log(['Operating system command executed.'])
   ;
    true % Error message handled by my_shell
  ).
processC(ashell,[C],_NVs,yes) :- 
  !, 
  my_shell(C,async,_S).
processC(Help,[],_NVs,yes) :- 
  (Help=h; Help=help), 
  !,
  display_help.
processC(Help,[KW],_NVs,yes) :- 
  (Help=h ; Help=help ; Help=apropos), 
  !,
  display_help(KW).
processC(save_ddb,[File],NVs,yes) :-
  !, 
  (my_file_exists(File) ->
    write_log('Info: File exists already. Overwrite? (y/n) [n]: '),
    user_input_string(Str),
    ((Str=="y" ; Str=="Y") ->
      Continue=yes
      ;
     Continue=no)
   ;
    Continue=yes
  ),
  (Continue==yes ->
    processC(save_ddb,[force,File],NVs,yes)
   ;
    true
  ).
processC(save_ddb,[force,File],_NVs,yes) :-
  (my_log(F,S) ->
    try_close(S), 
    retract(my_log(F,S))
   ;
    true
  ),
  output(OutputFlag),
%  set_flag(output,off),
  processC(log,[write,silent,File],[],yes),
  list_rules_wo_number(0,_DLs),
  list_constraint_rules,
  my_log(FS,SS),
  try_close(SS), 
  retract(my_log(FS,SS)),
  (var(F) ->
    true
   ;
    processC(log,[append,silent,F],[],yes)
  ),
  set_flag(output,OutputFlag).
processC(Cat,[File],_NVs,yes) :-
  (Cat=cat ; Cat=type), 
  !, 
  (my_file_exists(File) ->
    cat_file(File),
    nl_log
   ;
    write_warning_log(['File not found ''',File,'''.'])
   ).
processC(Edit,[File],_NVs,yes) :-
  (Edit=edit ; Edit=e), 
  !, 
%  (my_file_exists(File) ->
    (editor(Editor) ->
      atom_concat_list([Editor,' ',File],C),
      my_shell(C,async,_S)
     ;
      write_error_log(['External editor has not been set. Please set it with the command /set_editor <your_editor>.'])
    )
%   ;
%    write_warning_log(['File not found ''',File,'''.'])
%   ).
  .
processC(set_editor,[],_NVs,yes) :-
  !, 
  (editor(Editor)
   ->
    write_info_log(['Current external editor is ',Editor,'.'])
   ;
    write_error_log(['External editor has not been set. Please set it with the command /set_editor <your_editor>.'])
  ).
processC(set_editor,[Editor],_NVs,yes) :-
  !, 
%  (my_file_exists(Editor) ->
    set_flag(editor,Editor),
    write_info_verb_log(['Current external editor set to ',Editor,'.'])
%   ;
%    write_error_log(['External editor has not been found.'])
%   ).
  .
processC(Consult,Files,_NVs,yes) :-
  (Consult=consult ; Consult=c ; Consult=restore_ddb), 
  !, 
  (Files=[] ->
    write_warning_log(['Nothing consulted.'])
    ;
    reset_database,
    remove_duplicates_var(Files,UFiles),
    consult_DL_list(UFiles,Success),
    (Success -> compute_stratification ; true)),
    write_tapi_eot.
processC(Reconsult,Files,_NVs,yes) :-
  (Reconsult=reconsult; Reconsult=r), !, 
  (Files=[] ->
    write_warning_log(['Nothing reconsulted.'])
    ;
    abolishET, 
    remove_duplicates_var(Files,UFiles),
    consult_DL_list(UFiles,Success),
    (Success -> compute_stratification ; true)),
    write_tapi_eot.
processC(abolish,[],_NVs,yes) :-
  !, 
  drop_database.
%   reset_database,
%   reset_stratification,
%   write_info_verb_log(['Program abolished.']).
processC(abolish,[N/A],_NVs,yes) :-
  !,
  abolish_relation(N,A).
processC(abolish,[N],_NVs,yes) :-
  !,
  abolish_relation(N,_A).
processC(close_persistent,[N],_NVs,yes) :-
  !,
  close_persistent(N).
processC(drop_ic,[Constraint],NVs,yes) :-
  !,
  drop_ic(Constraint,NVs,_Error).
processC(drop_assertion,[Assertion],_NVs,yes) :-
  !,
  drop_assertion(Assertion).
processC(listing,[],_NVs,yes) :-
  !,
  list_rules(0,delim),
  display_elapsed_time.
processC(listing,[N/A],_NVs,yes) :-
  !,
  list_rules(N,A,0,delim),
  display_elapsed_time.
processC(listing,[N],_NVs,yes) :-
  atom(N),
  !,
  list_rules(N,0,delim),
  display_elapsed_time.
processC(listing,[H],_NVs,yes) :-
  H\=':-'(_H,_T),
  !,
  list_rules_from_head(H,0,delim),
  display_elapsed_time.
processC(listing,[R],_NVs,yes) :-
  !,
  list_rules_from_rule(R,0,delim),
  display_elapsed_time.
processC(listing_asserted,[],_NVs,yes) :-
  !,
  list_filtered_rules(0,delim,[asserted]),
  display_elapsed_time.
processC(listing_asserted,[N/A],_NVs,yes) :-
  !,
  list_filtered_rules(N,A,0,delim,[asserted]),
  display_elapsed_time.
processC(listing_asserted,[N],_NVs,yes) :-
  atom(N),
  !,
  list_filtered_rules(N,0,delim,[asserted]),
  display_elapsed_time.
processC(listing_asserted,[H],_NVs,yes) :-
  H\=':-'(_H,_T),
  !,
  list_filtered_rules_from_head(H,0,delim,[asserted]),
  display_elapsed_time.
processC(listing_asserted,[R],_NVs,yes) :-
  !,
  list_filtered_rules_from_rule(R,0,delim,[asserted]),
  display_elapsed_time.
processC(list_sources,[N/A],_NVs,yes) :-
  !,
  list_sources(N,A),
  display_elapsed_time.
processC(list_tables,[],_NVs,yes) :-
  !,
  list_tables.
processC(list_table_schemas,[],_NVs,yes) :-
  !,
  list_table_schemas.
processC(list_views,[],_NVs,yes) :-
  !,
  list_views.
processC(list_view_schemas,[],_NVs,yes) :-
  !,
  list_view_schemas.
processC(list_table_constraints,[Tablename],_NVs,yes) :-
  !,
  exist_table(Tablename),
  list_table_constraints(Tablename),
  write_tapi_eot.
processC(relation_schema,[Relation],_NVs,yes) :-
  !,
  exist_relation(Relation),
  list_relation_schema(Relation),
  write_tapi_eot.
processC(check_db,[],_NVs,yes) :-
  !,
  check_db.
processC(dbschema,[],_NVs,yes) :-
  !,
  list_schema.
processC(dbschema,[Connection:Relation],_NVs,yes) :-
%  exist_relation(Connection,Relation),
  !,
  my_odbc_relation_name(Connection,Relation,ODBCRelation),
  list_schema(Connection,ODBCRelation).
processC(dbschema,[Relation],_NVs,yes) :-
  current_db(Connection),
  my_odbc_relation_name(Connection,Relation,ODBCRelation),
  relation_exists(ODBCRelation),
  !,
  list_schema(Connection,ODBCRelation).
processC(dbschema,[Connection],_NVs,yes) :-
  !,
  list_schema(Connection,_Relation).
processC(list_et,[],_NVs,yes) :-
  !,
  list_et.
processC(list_et,[N/A],_NVs,yes) :-
  !,
  list_et(N/A).
processC(list_et,[N],_NVs,yes) :-
  !,
  list_et(N).
processC(clear_et,[],_NVs,yes) :- 
  !,
  verb_clear_et.
processC(builtins,[],_NVs,yes) :-
  !,
  list_builtins.
processC(datalog,[],_NVs,yes) :-
  !,
  set_flag(language(datalog)),
  write_info_verb_log(['Switched to Datalog interpreter.']).
processC(prolog,[],_NVs,yes) :-
  !,
  set_flag(language(prolog)),
  write_info_verb_log(['Switched to Prolog interpreter.']).
processC(sql,[],_NVs,yes) :-
  !,
  set_flag(language(sql)),
  write_info_verb_log(['Switched to SQL interpreter.']).
processC(ra,[],_NVs,yes) :-
  !,
  set_flag(language(ra)),
  write_info_verb_log(['Switched to RA interpreter.']).
% processC(sql,[Query],_NVs,yes) :-
%   !,
%   retract(simplification(S)),
%   assertz(simplification(on)),
%   reset_pred_id,
%   solve_sql_query(Query),
%   retract(simplification(on)), % WARNING: If Query fails, simplification mode is lost
%   assertz(simplification(S)).
processC(cd,[],_NVs,yes) :-
  !,
  start_path(Path),
  cd_path(Path).
processC(cd,[Path],_NVs,yes) :-
  !,
  cd_path(Path).
processC(pwd,[],_NVs,yes) :-
  !,
  pwd_path.
processC(LS,[],_NVs,yes) :-
  (LS=ls; LS=dir),
  !,
  ls.
processC(LS,[P],_NVs,yes) :-
  (LS=ls; LS=dir),
  !,
  ls(P).
processC(RM,[FileName],_NVs,yes) :-
  (RM=rm; RM=del),
  !,
  rm_file(FileName).
processC(safety_warnings,[],_NVs,yes) :-
  !, 
  safety_warnings(Switch),
  write_info_log(['Safety warnings are ', Switch, '.']).
processC(safety_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(safety_warnings,'Safety warnings are',Switch).
processC(singleton_warnings,[],_NVs,yes) :-
  !, 
  singleton_warnings(Switch),
  write_info_log(['Singleton warnings are ', Switch, '.']).
processC(singleton_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(singleton_warnings,'Singleton warnings are',Switch).
processC(type_casting,[],_NVs,yes) :-
  !, 
  type_casting(Switch),
  write_info_log(['Automatic type casting is ', Switch, '.']).
processC(type_casting,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(type_casting,'Automatic type casting is',Switch).
processC(undef_pred_warnings,[],_NVs,yes) :-
  !, 
  undef_pred_warnings(Switch),
  write_info_log(['Undefined predicate warnings are ', Switch, '.']).
processC(undef_pred_warnings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(undef_pred_warnings,'Undefined predicate warnings are',Switch).
processC(show_compilations,[],_NVs,yes) :-
  !, 
  show_compilations(Switch),
  write_info_log(['Display of compilations is ', Switch, '.']).
processC(show_compilations,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(show_compilations,'Display of compilations is',Switch).
processC(show_sql,[],_NVs,yes) :-
  !, 
  show_sql(Switch),
  write_info_log(['Display of externally-processed SQL is ', Switch, '.']).
processC(show_sql,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(show_sql,'Display of externally-processed SQL is',Switch).
processC(fp_info,[],_NVs,yes) :-
  !, 
  fp_info(Switch),
  write_info_log(['Display of fixpoint info is ', Switch, '.']).
processC(fp_info,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(fp_info,'Display of fixpoint info is',Switch).
processC(running_info,[],_NVs,yes) :-
  !, 
  running_info(Switch),
  write_info_log(['Display of running info is ', Switch, '.']).
processC(running_info,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(running_info,'Display of running info is',Switch).
processC(verbose,[],_NVs,yes) :-
  !, 
  verbose(Switch),
  write_info_log(['Verbose output is ', Switch, '.']).
processC(verbose,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(verbose,'Verbose output is',Switch).
processC(duplicates,[],_NVs,yes) :-
  !, 
  duplicates(Switch),
  write_info_log(['Duplicates are ', Switch, '.']).
processC(duplicates,[Switch],_NVs,yes) :-
  !,
  (duplicates(off), Switch==on
   ->
%    my_idx_retractall(complete_flag(_P,_G,_CF,_CId)) 
    clear_et
   ;
    true),
  process_set_binary_flag(duplicates,'Duplicates are',Switch).
processC(compact_listings,[],_NVs,yes) :-
  !, 
  compact_listings(Switch),
  write_info_log(['Compact listings are ', Switch, '.']).
processC(compact_listings,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(compact_listings,'Compact listings are',Switch).
processC(prompt,[],_NVs,yes) :-
  !, 
  prompt(Switch),
  write_info_log(['Current prompt is set to ''', Switch, '''.']).
processC(prompt,[Switch],_NVs,yes) :-
  !, 
  ((Switch == des ; Switch == des_db ; Switch == plain ; Switch == prolog ; Switch == no) ->
    set_flag(prompt,Switch),
    (Switch==no -> prompt(_,'') ; prompt(_,'|: ')),
    exec_if_verbose_on(processC(prompt,[],_,_))
    ;
    write_error_log(['Incorrect switch. Use ''des'', ''des_db'', ''plain'' or ''no'''])
    ).
processC(timing,[],_NVs,yes) :-
  !, 
  timing(Switch),
  write_info_log(['Elapsed time display is ', Switch, '.']).
processC(timing,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off ; LSwitch == detailed ) ->
    retractall(timing(_)), 
    assertz(timing(LSwitch)),
    exec_if_verbose_on(processC(timing,[],_,_))
    ;
    write_error_log(['Incorrect switch. Use ''on'', ''off'' or ''detailed'''])
    ).
processC(format_timing,[],_NVs,yes) :-
  !, 
  format_timing(Switch),
  write_info_log(['Formatting of timing is ', Switch, '.']).
processC(format_timing,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(format_timing,'Formatting of time is',Switch).
processC(pretty_print,[],_NVs,yes) :-
  !, 
  pretty_print(Switch),
  write_info_log(['Pretty print is ', Switch, '.']).
processC(pretty_print,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(pretty_print,'Pretty print is',Switch).
processC(safe,[],_NVs,yes) :-
  !, 
  safe(Switch),
  write_info_log(['Program transformation for safety is ', Switch, '.']).
processC(safe,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(safe,'Program transformation for safety is',Switch).
processC(simplification,[],_NVs,yes) :-
  !, 
  simplification(Switch),
  write_info_log(['Program simplification is ', Switch, '.']).
processC(simplification,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(simplification,'Program simplification is',Switch).
processC(reorder_goals,[],_NVs,yes) :-
  !, 
  reorder_goals(Switch),
  write_info_log(['Goal reordering is ', Switch, '.']).
processC(reorder_goals,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(reorder_goals,'Goal reordering is',Switch).
processC(unfold,[],_NVs,yes) :-
  !, 
  unfold(Switch),
  write_info_log(['Program unfolding is ', Switch, '.']).
processC(unfold,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(unfold,'Program unfolding is',Switch).
processC(statistics,[],_NVs,yes) :-
  !, 
  my_statistics(Switch),
  write_info_log(['Statistics collection is ', Switch, '.']),
  (Switch == on
   -> 
    display_statistics
   ;
    true).
processC(statistics,[Switch],_NVs,yes) :-
  !, 
  my_statistics(OldSwitch),
  process_set_binary_flag(my_statistics,'Statistics collection is',Switch),
  (Switch == on
   -> 
    (OldSwitch == off
     ->
      reset_statistics
     ;
      true
    ),
    display_statistics
   ;
    true).
processC(host_statistics,[Kw],_NVs,yes) :-
  !, 
  display_host_statistics(Kw).
processC(start_stopwatch,[],_NVs,yes) :-
  !, 
  start_stopwatch,
  verb_display_stopwatch.
processC(stop_stopwatch,[],_NVs,yes) :-
  !, 
  stop_stopwatch,
  verb_display_stopwatch.
processC(reset_stopwatch,[],_NVs,yes) :-
  !, 
  reset_stopwatch,
  verb_display_stopwatch.
processC(display_stopwatch,[],_NVs,yes) :-
  !, 
  display_stopwatch.
processC(license,[],_NVs,yes) :-
  !, 
  processC(cat,['license/COPYING'],_,yes),
  processC(cat,['license/COPYING.LESSER'],_,yes).
processC(output,[],_NVs,yes) :-
  !, 
  output(Switch),
  write_info_log(['Output is ', Switch, '.']).
processC(output,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(output(_)), 
    assertz(output(LSwitch)),
    exec_if_verbose_on(processC(output,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(display_banner,[],_NVs,yes) :-
  !, 
  display_banner(Switch),
  write_info_log(['Display banner is ', Switch, '.']).
processC(display_banner,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(display_banner,'Display banner is',Switch).
processC(display_answer,[],_NVs,yes) :-
  !, 
  display_answer(Switch),
  write_info_log(['Display of answers is ', Switch, '.']).
processC(display_answer,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(display_answer(_)), 
    assertz(display_answer(LSwitch)),
    exec_if_verbose_on(processC(display_answer,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(display_nbr_of_tuples,[],_NVs,yes) :-
  !, 
  display_nbr_of_tuples(Switch),
  write_info_log(['Display of the number of computed tuples is ', Switch, '.']).
processC(display_nbr_of_tuples,[Switch],_NVs,yes) :-
  process_set_binary_flag(display_nbr_of_tuples,'Display of the number of computed tuples is',Switch).
processC(order_answer,[],_NVs,yes) :-
  !, 
  order_answer(Switch),
  (Switch == on -> Message = ordered ; Message = 'not ordered by default'),
  write_info_log(['Display of answers is ', Message, '.']).
processC(order_answer,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(order_answer(_)), 
    assertz(order_answer(LSwitch)),
    exec_if_verbose_on(processC(order_answer,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(multiline,[],_NVs,yes) :-
  !, 
  multiline(Switch),
  write_info_log(['Multiline input is ', Switch, '.']).
processC(multiline,[Switch],_NVs,yes) :-
  !, 
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(multiline(_)), 
    assertz(multiline(LSwitch)),
    exec_if_verbose_on(processC(multiline,[],_,_))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(indexing,[],_NVs,yes) :-
  !, 
  indexing(Switch),
  write_info_log(['Hash indexing on memo tables is ', Switch, '.']).
processC(indexing,[Switch],NVs,yes) :-
  !, 
  indexing(OldSwitch),
  to_lowercase(Switch,LSwitch),
  ((LSwitch == on ; LSwitch == off) ->
    retractall(indexing(_)), 
    assertz(indexing(LSwitch)),
    exec_if_verbose_on(processC(indexing,[],_,_)),
    (OldSwitch == LSwitch -> true ; processC(clear_et,[],NVs,yes))
    ;
   write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
processC(check,[],_NVs,yes) :-
  !, 
  check_ic(Switch),
  write_info_log(['Integrity constraint checking is ', Switch, '.']).
processC(check,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(check_ic,'Integrity constraint checking is',Switch).
processC(nulls,[],_NVs,yes) :-
  !, 
  nulls(Switch),
  write_info_log(['Null values are ', Switch, '.']).
processC(nulls,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(nulls,'Nulls are',Switch).
processC(hypothetical,[],_NVs,yes) :-
  !, 
  hypothetical(Switch),
  write_info_log(['Hypothetical queries are ', Switch, '.']).
processC(hypothetical,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(hypothetical,'Hypothetical queries are',Switch).
processC(optimize,[],_NVs,yes) :-
  !, 
  processC(optimize_cc,[],_,_),
  processC(optimize_ep,[],_,_),
%  processC(optimize_edb,[],_,_),
  processC(optimize_nrp,[],_,_),
  processC(optimize_st,[],_,_),
  processC(indexing,[],_,_).
processC(optimize_cc,[],_NVs,yes) :-
  !, 
  optimize_cc(Switch),
  write_info_log(['Complete computations optimization is ', Switch, '.']).
processC(optimize_cc,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(optimize_cc,'Complete computations optimization is',Switch).
% processC(optimize_edb,[],_NVs,yes) :-
%   !, 
%   optimize_edb(Switch),
%   write_info_log(['Extensional database optimization is ', Switch, '.']).
% processC(optimize_edb,[Switch],_NVs,yes) :-
%   !, 
%   process_set_binary_flag(optimize_edb,'Extensional database optimization is',Switch).
processC(optimize_ep,[],_NVs,yes) :-
  !, 
  optimize_ep(Switch),
  write_info_log(['Extensional predicate optimization is ', Switch, '.']).
processC(optimize_ep,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(optimize_ep,'Extensional predicate optimization is',Switch).
processC(optimize_nrp,[],_NVs,yes) :-
  !, 
  optimize_nrp(Switch),
  write_info_log(['Non-recursive predicates optimization is ', Switch, '.']).
processC(optimize_nrp,[Switch],_NVs,yes) :-
  !, 
  optimize_nrp(OldSwitch),
  process_set_binary_flag(optimize_nrp,'Non-recursive predicates optimization is',Switch),
  (OldSwitch==on, Switch==off -> retractall(complete_flag(_,_,_,_,_)) ; true).
processC(optimize_st,[],_NVs,yes) :-
  !, 
  optimize_st(Switch),
  write_info_log(['Stratum optimization is ', Switch, '.']).
processC(optimize_st,[Switch],_NVs,yes) :-
  !, 
  optimize_st(OldSwitch),
  process_set_command_flag(optimize_st,'Stratum optimization is',Switch,[on,off,max]),
  (OldSwitch == Switch -> true ; compute_stratification).
processC(strata,[],_NVs,yes) :-
  !, 
  display_strata.
processC(strata,[N/A],_NVs,yes) :-
  !, 
  display_strata_for(N/A).
processC(strata,[N],_NVs,yes) :-
  !, 
  display_strata_for(N/_A).
processC(dependent_relations,[RelationName/Arity],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_relation_name(Connection,RelationName,ODBCRelationName),
  dependent_relations(ODBCRelationName/Arity,Preds),
  write_list_log(Preds),
  write_tapi_eot.
processC(dependent_relations,[RelationName],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_relation_name(Connection,RelationName,ODBCRelationName),
  dependent_relations(ODBCRelationName,RelationNames),
  write_list_log(RelationNames),
  write_tapi_eot.
processC(referenced_relations,[RelationName/Arity],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_relation_name(Connection,RelationName,ODBCRelationName),
  referenced_relations(ODBCRelationName/Arity,Preds),
  write_list_log(Preds),
  write_tapi_eot.
processC(referenced_relations,[RelationName],_NVs,yes) :-
%  exist_relation(RelationName),
  current_db(Connection),
  my_odbc_relation_name(Connection,RelationName,ODBCRelationName),
  referenced_relations(ODBCRelationName,RelationNames),
  write_list_log(RelationNames),
  write_tapi_eot.
processC(pdg,[],_NVs,yes) :-
  !,
  display_pdg.
processC(pdg,[N/A],_NVs,yes) :-
  !, 
  display_sub_pdg_for(N/A).
processC(pdg,[N],_NVs,yes) :-
  !, 
  display_sub_pdg_for(N/_A).
processC(rdg,[],_NVs,yes) :-
  !,
  display_rdg.
processC(rdg,[N/A],_NVs,yes) :-
  !, 
  display_sub_rdg_for(N/A).
processC(rdg,[N],_NVs,yes) :-
  !, 
  display_sub_rdg_for(N/_A).
processC(Log,[],_NVs,yes) :-
  (Log==log -> (Flag=my_log(F,_), M='') ; (Log==logiql_log -> Flag=logiql_log(F,_), M='LogiQL ')), 
  !,
  (call(Flag) -> 
   write_info_log([M,'Currently logging to ',F,'.'])
   ;
   write_info_log([M,'Logging disabled.'])
  ).
processC(Log,[Mode,Output,File],_NVs,yes) :-
  (Log==log -> (Flag=my_log(F,S), M='', N='/nolog') ; (Log==logiql_log -> Flag=logiql_log(F,S), M='LogiQL ', N='/logiql_nolog')), 
  !, 
  (call(Flag) ->
    write_warning_log(['Currently ',M,'logging to ''',F,'''. First, use ',N,' for disabling the current log.'])
   ;
   (my_absolute_filename(File,F),
    my_dir_file(F,AP,_FN),
    (my_directory_exists(AP) ->
      open(F,Mode,S),
      assertz(Flag), 
      (Output==silent
       ->
       true
       ;
       write_info_verb_log([M,'Logging enabled to ',F,'.']),
       (M='' -> true ; set_flag(logiql,on))
      )
     ;
      write_warning_log(['Directory ',AP,' does not exist.'])
    )
   )
  ). 
processC(Nolog,[],_NVs,yes) :-
  (Nolog==nolog -> (Flag=my_log(F,S), M='') ; (Nolog==logiql_nolog -> Flag=logiql_log(F,S), M='LogiQL ')), 
  !, 
  (call(Flag) ->
   (flush_output(S),
    try_close(S), 
    retract(Flag), 
    write_info_verb_log([M,'Logging to ', F, ' disabled.']))
   ;
    write_warning_log([M,'Logging already disabled.'])
  ).
processC(Process,[F|Params],_NVs,yes) :-
  (Process=process; Process=p), 
  !,
  push_param_vector,
  set_param_vector(Params),
  process_batch(F),
  pop_param_vector.
processC(repeat(N),[Input],_NVs,yes) :-
  !,
  name(Input,InputStr),
  length(L,N),
  findall(_,(member(_,L),process_input(InputStr,_Continue,no_nl)),_).
processC(set_default_parameter,[ParamVectorIndex,Value],_NVs,yes) :-
  !,
  (param_vector_i(ParamVectorIndex,_Value)
   ->
    true
   ;
    set_param_vector_i(ParamVectorIndex,Value)).
processC(version,[],_NVs,yes) :-
  !, 
  des_version(V), 
  write_info_log(['DES version ',V,'.']).
processC(prolog_system,[],_NVs,yes) :-
  !, 
  prolog_system(_P,V), 
  write_info_log(['Prolog engine: ',V,'.']).
processC(status,[],_NVs,yes) :-
  !, 
  display_status.
processC(restore_default_status,[],NVs,C) :-
  !, 
  processC(reset,[],NVs,C).
processC(reset,[],_NVs,yes) :-
  !, 
  set_initial_status.
processC(open_db,[Connection|Opts],_NVs,yes) :-
  !, 
  (opened_db(Connection)
   ->
    current_db(CurrentConnection),
    (CurrentConnection\==Connection
     ->
      write_warning_log(['The database ''',Connection,''' is opened already.']),
      processC(use_db,[Connection],[],yes)
     ;
      write_warning_log(['The database ''',Connection,''' is opened already and is the current one.'])
    )
   ;
    my_open_odbc(Connection,Opts),
    (opened_db(Connection,_Handle,DBMS)
     ->
      set_flag(current_db(Connection,DBMS)),
      enable_rdb_datasource(Connection),
      refresh_db_metadata,
      write_tapi_success
     ;
      true % Error message displayed already 
    )
  ).
processC(close_db,[],NVs,yes) :-
  !, 
  current_db(Connection),
  processC(close_db,[Connection],NVs,yes).
processC(close_db,[Connection],_NVs,yes) :-
  !, 
  (Connection=='$des'
   ->
    write_warning_log(['Default database ''$des'' cannot be closed.'])
   ;
    (my_persistent(Connection,_PredSchema)
     ->
      write_error_log(['Cannot close connection. There are persistent predicates on it.'])
     ;
      my_close_odbc(Connection),
      processC(clear_et,[],[],yes),
      compute_stratification,
      write_tapi_success
    )
  ).
processC(current_db,[],_NVs,yes) :-
  !, 
  current_db(Connection,DBMS),
  write_notapi_info_log(['Current database is ''',Connection,'''. DBMS: ',DBMS]),
  write_tapi_log_list([Connection,nl,DBMS,nl]).
processC(refresh_db,[],_NVs,yes) :-
  !, 
  current_db(Connection),
  refresh_db_metadata,
  (Connection=='$des'
   ->
    write_notapi_info_log(['Default database ''$des'' refreshed.'])
   ;
    write_notapi_info_log(['Local metadata from external database refreshed.'])),
  write_tapi_success.
processC(show_dbs,[],_NVs,yes) :-
  !, 
  setof([Connection,nl],opened_db(Connection),LConnectionLines),
  concat_lists(LConnectionLines,ConnectionLines),
  write_log_list(ConnectionLines),
  write_tapi_eot.
processC(list_persistent,[],_NVs,yes) :-
  !, 
  my_nf_setof([Connection:PredSchema,nl],my_persistent(Connection,PredSchema),LConnectionLines),
  concat_lists(LConnectionLines,ConnectionLines),
  write_log_list(ConnectionLines),
  write_tapi_eot.
processC(list_modes,[],NVs,yes) :-
  !, 
  processC(list_modes,[_],NVs,yes).
processC(list_modes,[N/A],_NVs,yes) :-
  !, 
  findall([PredModes,nl],(my_modes(N/A,Modes), PredModes=..[N|Modes]),LLines),
  my_mergesort(LLines,OLLines),
  concat_lists(OLLines,Lines),
  write_log_list(Lines),
  write_tapi_eot.
processC(list_modes,[N],NVs,yes) :-
  !, 
  processC(list_modes,[N/_A],NVs,yes).
processC(relation_exists,[Relation],_NVs,yes) :-
  !, 
  (relation_exists(Relation)
   ->
   write_tapi_true
   ;
   write_tapi_false
  ).
processC(is_empty,[Relation],_NVs,yes) :-
  !, 
  exist_relation(Relation),
  (is_empty_relation(Relation)
   ->
   write_tapi_true
   ;
   write_tapi_false
  ).
processC(use_ddb,[],NVs,Cont) :-
  !, 
  processC(use_db,['$des'],NVs,Cont).
processC(use_db,[Connection],NVs,yes) :-
  !, 
  (current_db(Connection) 
   ->
    write_warning_log(['Database already in use.'])
   ;
    (opened_db(Connection,_Handle,DBMS)
     ->
      set_flag(current_db(Connection,DBMS)),
      % When switching between databases (either ODBC connection or default $des,
      % ET must be cleared since it may hold results from the origin database
      processC(clear_et,[],NVs,yes),
      push_flag(verbose,on,CurrentValue),
      compute_stratification,
      pop_flag(verbose,CurrentValue),
      write_info_verb_log(['Current database changed to ''',Connection,'''.'])
     ;
      write_warning_log(['Database is not opened yet. Opening it ...']),
      processC(open_db,[Connection],NVs,yes)
      %write_error_log(['Database is not opened yet. Use /open_db'])
    )
  ).
processC(trace_sql,[ViewName],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_relation_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  trace_sql(ODBCViewName,preorder).
processC(trace_sql,[ViewName,Ordering],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_relation_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  trace_sql(ODBCViewName,Ordering).
processC(trace_datalog,[Query],NVs,yes) :-
  !, 
  functor(Query,F,A), 
  exist_user_predicate(F/A),
  trace_datalog(Query,NVs,preorder).
processC(trace_datalog,[Query,Ordering],NVs,yes) :-
  !, 
  trace_datalog(Query,NVs,Ordering).
% processC(debug_dl,[Name/Arity,File],_NVs,yes) :-
%   !, 
%   (my_file_exists(File) ->
%     debug_dl(Name,Arity,File)
%    ;
%     write_warning_log(['File not found ''',File,'''.'])
%    ).
processC(debug_datalog,[Goal],NVs,yes) :-
  !, 
%  debug_datalog_goal_level(Goal,'p',NVs).
  Level='p',
  processC(debug_datalog,[Goal,Level],NVs,yes).
processC(debug_datalog,[Goal,Level],NVs,yes) :-
  !, 
%  functor(Goal,F,A),
%  exist_user_predicate(F/A),
  debug_datalog_goal_level(Goal,Level,NVs).
processC(debug_sql,[ViewName|Options],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_relation_name(Connection,ViewName,ODBCViewName),
  exist_view(ODBCViewName),
  debug_sql(ODBCViewName,Options).
processC(test_case,[ViewName|Options],_NVs,yes) :-
  !, 
  current_db(Connection),
  my_odbc_relation_name(Connection,ViewName,ODBCViewName),
  process_test_case(ODBCViewName,Options).
processC(tc_size,[],_NVs,yes) :-
  !, 
  tc_size(Min,Max),
  write_info_log(['Test case size is set between ', Min, ' and ', Max, '.']).
processC(tc_size,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min>0, Min=<Max ->
    set_flag(tc_size,Min,Max),
    exec_if_verbose_on(processC(tc_size,[],NVs,_))
   ;
    write_error_log(['Incorrect parameter(s).'])).
processC(tc_domain,[],_NVs,yes) :-
  !, 
  tc_domain(Min,Max),
  write_info_log(['Test case domain is set between ', Min, ' and ', Max, '.']).
processC(tc_domain,[Min,Max],NVs,yes) :-
  !, 
  (number(Min), number(Max), Min=<Max ->
    set_flag(tc_domain,Min,Max),
    exec_if_verbose_on(processC(tc_domain,[],NVs,_))
   ;
    write_error_log(['Incorrect parameter(s).'])).
processC(development,[],_NVs,yes) :-
  !, 
  development(Switch),
  write_info_log(['Development listings are ', Switch, '.']).
processC(development,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(development,'Development listings are',Switch).
processC(list_lex_datalog,[],_NVs,yes) :-
  !, 
  setof(KW,datalog_keyword(KW),Cs),
  findall(_,(member(C,Cs),write_log_list([C,nl])),_).
processC(list_lex_sql,[],_NVs,yes) :-
  !, 
  setof(KW,sql_identifier(KW),Cs),
  findall(_,(member(C,Cs),write_log_list([C,nl])),_).
processC(list_lex_ra,[],_NVs,yes) :-
  !, 
  setof(KW,ra_keyword(KW),Cs),
  findall(_,(member(C,Cs),write_log_list([C,nl])),_).
processC(list_lex_cmds,[],_NVs,yes) :-
  !, 
  setof(Com,A^B^C^D^E^F^command(A,B,C,Com,D,E,F),Cs),
  findall(_,(member(C,Cs),write_log_list(['/',C,nl])),_).
processC(list_lex_cmd_opt,[],_NVs,yes) :-
  !, 
  write_log_list([add,nl,append,nl,des,nl,des_db,nl,detailed,nl,display,nl,no,nl,off,nl,plain,nl,postorder,nl,preorder,nl,prolog,nl,replace,nl,runtime,nl,total_runtime,nl,trust_file,nl,trust_tables,nl,write,nl,yes,nl]).
processC(list_lex_cmds,[acide],_NVs,yes) :-
  !, 
  setof(Com,A^B^C^D^E^F^command(A,B,C,Com,D,E,F),Cs),
  findall(_,(member(C,Cs),write_log_list(['              <string>/',C,'</string>',nl])),_).
processC(Logiql,[],_NVs,yes) :-
  (Logiql=logiql ; Logiql=lq),
  !, 
  logiql(Switch),
  write_info_log(['LogiQL output is ', Switch, '.']).
processC(Logiql,[Switch],_NVs,yes) :-
  (Logiql=logiql ; Logiql=lq),
  !, 
  process_set_binary_flag(logiql,'LogiQL output is',Switch).
processC(logiql_cmd,[],_NVs,yes) :-
  !, 
  logiql_cmd(Switch),
  write_info_log(['LogiQL commands are ', Switch, '.']).
processC(logiql_cmd,[Switch],_NVs,yes) :-
  !, 
  process_set_binary_flag(logiql_cmd,'LogiQL commands are',Switch),
  (logiql_cmd(on) -> set_flag(logiql,on) ; true).
processC(Command,_L,_NVs,yes) :-
  !, 
  write_error_log(['Unknown command or incorrect number of arguments. Use ''/help'' or ''/help keyword'' for help.']),
  display_object_alternatives(command,Command).

:-dynamic(my_paused_log/2).
pause_log :-
  retract(my_log(F,S)),
  !,
  assertz(my_paused_log(F,S)).
pause_log.
 
resume_log :-
  retract(my_paused_log(F,S)),
  !,
  assertz(my_log(F,S)).
resume_log.

refresh_db_metadata :-
  processC(clear_et,[],[],yes),
  push_flag(verbose,on,CurrentValue),
%  create_dual_table_if_not_exist(Connection),
  compute_stratification,
  pop_flag(verbose,CurrentValue).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMAND FLAGS
  
% Sets a binary flag (on, off values). Called from processC
process_set_binary_flag(FlagFunctor,Message,Switch) :-
  to_lowercase(Switch,LSwitch),
  Flag=..[FlagFunctor,VarSwitch],
  ((LSwitch == on; LSwitch == off) ->
    (LSwitch == on, call(Flag), VarSwitch==on ->
      write_info_log(['',Message,' already enabled.'])
     ;
      (LSwitch == off, call(Flag), VarSwitch==off ->
        write_info_log(['',Message,' already disabled.'])
       ;
        set_flag(FlagFunctor,LSwitch),
        write_info_verb_log(['',Message,' ', Switch, '.'])
      )
    )
   ;
    write_error_log(['Incorrect switch. Use ''on'' or ''off'''])
  ).
  
% Switch: Is the requested new Switch
process_set_command_flag(FlagFunctor,Message,Switch,PossibleValues) :-
  to_lowercase(Switch,LSwitch),
  Flag=..[FlagFunctor,VarSwitch],
  call(Flag), 
  (member(LSwitch,PossibleValues)
   ->
    (LSwitch == VarSwitch ->
      (LSwitch == on
       ->
        write_info_log(['',Message,' already enabled.'])
       ;
       (LSwitch == off
        ->
         write_info_log(['',Message,' already disabled.'])
        ;  
         write_info_log(['',Message,' already set.'])
       )
      )
     ;
      set_flag(FlagFunctor,LSwitch),
      write_info_verb_log(['',Message,' ', Switch, '.'])
     )
   ;
    write_error_log(['Incorrect switch. Possible values: ',PossibleValues])
  ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

% Help system
% Commands
% CategoryOrder allows ordered listings. It should not be viewed as a category identifier
% command_category(CategoryOrder,CategoryName).
command_category(10,'DES Database').
command_category(20,'ODBC Database').
command_category(30,'Debugging and Test Case Generation').
command_category(40,'Tabling').
command_category(50,'Operating System').
command_category(60,'Logging').
command_category(70,'Informative').
command_category(80,'Query Languages').
command_category(90,'TAPI').
command_category(100,'Settings').
command_category(110,'Timing').
command_category(120,'Statistics').
command_category(130,'Miscellanea').
command_category(140,'Implementors').

% command(CategoryOrder,CommandOrder,CommandType:(c)ommand|(s(Cmd))horthand|(s(Cmd))ynonim),Command,Arguments,ShortDescription,ExtendedDescription)
% CommandOrder allows ordered listings. It should not be viewed as a command identifier

% DES Database:
command(10,10,c,'[','Filenames]','Consult Datalog files, abolishing previous rules','Load the Datalog programs found in the comma-separated list [Filenames], discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. Arguments in the list are comma-separated').
command(10,20,c,'[+','Filenames]','Consult Datalog files, keeping previous rules','Load the Datalog programs found in the comma-separated list Filenames, keeping rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed.').
command(10,30,c,abolish,'','Abolish the Datalog database','Delete the Datalog database. This includes all the local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Integrity constraints, and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,40,c,abolish,'Name','Abolish the predicates matching Name','Delete the predicates matching Name. This includes all their local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Their integrity constraints, and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,50,c,abolish,'Name/Arity','Abolish the predicate matching the pattern','Delete the predicates matching the pattern Name/Arity. This includes all their local rules (including those which are the result of SQL compilations) and external rules (persistent predicates). Their integrity constraints, and SQL table and view definitions are removed. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,60,c,assert,'Head:-Body','Assert a rule. :-Body is optional (for facts)','Add a Datalog rule. If Body is not specified, it is simply a fact. Rule order is irrelevant for Datalog computation. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,65,c,close_persistent,'Name','Close the connection to the persistent predicate Name','Close the connection to the persistent predicate Name. External relations supporting its persistence are kept but the predicate is no longer visible. Its type information is kept in the local database metadata').
command(10,70,c,consult,'Filename','Consult a Datalog file, abolishing previous rules','Load the Datalog program found in the file Filename, discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. The default extension .dl for Datalog programs can be omitted').
command(10,75,s(consult,'Filename'),'c','Filename','Shorthand for /consult Filename','Load the Datalog program found in the file Filename, discarding the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed. The default extension .dl for Datalog programs can be omitted').
command(10,80,c,check_db,'','Check database consistency w.r.t. declared integrity constraints','Check database consistency w.r.t. declared integrity constraints (types, existency, primary key, candidate key, foreign key, functional dependency, and user-defined). Displays a report with the outcome').
command(10,90,c,des,'Input','Force DES to solve Input','Force DES to solve Input. If Input is an SQL query, DES solves it instead of relying on external DBMS solving. This allows to try the more expressive queries which are available in DES (as, e.g., hypothetical and non-linear recursive queries)').
command(10,100,c,drop_ic,'Constraint','Drop an integrity constraint','Drop the specified integrity constraint, which starts with \':-\' (either one of :-nn(Table,Columns), :-pk(Table,Columns), :-ck(Table,Columns), :-fk(Table,Columns,RTable,RColumns), :-fd(Table,Columns,DColumns), :-Goal, where Goal specifies a user-defined integrity constraint). Only one constraint can be dropped at a time. TAPI enabled').
command(10,110,c,drop_assertion,'Assertion','Drop an assertion','Drop the specified assertion, which starts with \':-\'. So far, there is only support for :-persistent(Schema[,Connection]). Where Schema is the ground atom describing the predicate (predicate and argument names, as: pred_name(arg_name1,...,arg_nameN)) that has been made persistent on an external DBMS via ODBC, and Connection is an optional connection name for the external RDB. Only one assertion can be dropped at a time').
command(10,120,c,listing,'','List the loaded Datalog rules','List the loaded Datalog rules. Neither integrity constraints nor SQL views and metadata are displayed').
command(10,125,c,listing,'Name','List the loaded Datalog rules matching Name','List the loaded Datalog rules matching Name. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,130,c,listing,'Name/Arity','List Datalog rules matching the pattern','List the loaded Datalog rules matching the pattern Name/Arity. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,135,c,listing,'Head','List Datalog rules whose head is subsumed by Head','List the Datalog loaded rules whose heads are subsumed by the head Head. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,140,c,listing,'Head:-Body','List Datalog rules that are subsumed by Head:-Body','List the Datalog loaded rules that are subsumed by Head:-Body. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,145,c,listing_asserted,'','List the asserted Datalog rules','List the Datalog rules that have been asserted with command. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,150,c,listing_asserted,'Name/Arity','List the asserted Datalog rules matching the pattern','List the Datalog rules matching the given pattern Name/Arity that have been asserted with command. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,155,c,listing_asserted,'Head','List the asserted Datalog rules whose head is subsumed by Head','List the the Datalog rules that have been asserted with command whose heads are subsumed by the head Head. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,160,c,listing_asserted,'Head:-Body','List the asserted Datalog rules that are subsumed by Head:-Body','List the the Datalog rules that have been asserted with command that are subsumed by Head:-Body. Rules from consulted files are not listed. Neither integrity constraints nor SQL views and metadata are displayed. TAPI enabled').
command(10,170,c,list_modes,'','List the expected modes for unsafe predicates in order to be correctly computed','List the expected modes for unsafe predicates in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,180,c,list_modes,'Name','List expected modes, if any, for predicates with name Name in order to be correctly computed','List expected modes, if any, for predicates with name Name in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,185,c,list_modes,'Name/Arity','List expected modes, if any, for the given predicate Name/Arity in order to be correctly computed','List expected modes, if any, for the given predicate Name/Arity in order to be correctly computed. Modes can be \'i\' (for an input argument) and \'o\' (for an output argument)').
command(10,190,c,list_persistent,'','List persistent predicates','List persistent predicates along with their ODBC connection names. TAPI enabled').
command(10,195,c,list_sources,'Name/Arity','List the sources of the Datalog rules matching the pattern','List the sources of the Datalog rules matching the pattern Name/Arity').
command(10,200,c,reconsult,'Filename','Consult a Datalog file, keeping previous rules','Load a Datalog program found in the file Filename, keeping the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,205,s(reconsult,'Filename'),'r','Filename','Shorthand for /reconsult Filename','Load a Datalog program found in the file Filename, keeping the rules already loaded. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,210,c,restore_ddb,'Filename','Restore the Datalog database in Filename','Restore the Datalog database in the given Filename (same as consult). Constraints (type, not nullables, primary key, candidate key, functional dependency, foreign key, and user-defined) are also restored, if present in Filename').
command(10,220,c,retract,'Head:-Body','Retract a rule. :-Body is optional (for facts)','Delete the first Datalog rule that unifies with Head:-Body (or simply with Head, if Body is not specified. In this case, only facts are deleted). The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,230,c,retractall,'Head','Retract all rules matching the given head','Delete all the Datalog rules whose heads unify with Head. The extension table is cleared, and the predicate dependency graph and strata are recomputed').
command(10,240,c,save_ddb,'[force] Filename','Save the current Datalog database to a file','Save the current Datalog database to the file Filename. If option \'force\' is included, no question is asked to the user should the file exists already. Constraints (type, not nullables, primary key, candidate key, functional dependency, foreign key, and user-defined) are also saved').

% ODBC Database:
command(20,10,c,open_db,'Conn [Opts]','Open and set the current ODBC connection','Open and set the current ODBC connection to Name, where Opts=[user(Username)] [password(Password)]. This connection must be already defined at the OS layer. TAPI enabled').
command(20,20,c,close_db,'','Close the current ODBC connection','Close the current ODBC connection. TAPI enabled').
command(20,25,c,close_db,'Name','Close the ODBC connection Name','Close the ODBC connection denoted as Name. TAPI enabled').
command(20,30,c,current_db,'','Display the current ODBC connection info','Display the current ODBC connection name and DSN provider. TAPI enabled').
command(20,35,c,refresh_db,'','Refresh local database metadata','Refresh local metadata from the current external database (only for external DB\'s), clear the cache, and recompute the PDG and strata. TAPI enabled').
command(20,40,c,show_dbs,'','Display the current opened databases','Display the current opened databases, handled by DES and external DBMS\'s').
command(20,50,c,use_db,'Name','Make Name the current database','Make Name the current database, handled by DES or by an external DBMS. If it is not open already, it is automatically opened').
command(20,60,c,use_ddb,'','Make $des the current database','Make $des (the default deductive database) the current database. Shorthand for /use_db $des').

% Debugging and Test Case Generation:
command(30,10,c,debug_datalog,'Goal [Level]','Debug a Datalog basic goal','Start the debugger for the basic goal Goal at predicate or clause levels, which is indicated with the options p and c for Level, respectively. Default is p').
%command(30,20,c,debug_dl,'Name/Arity File','Debug a Datalog relation included in a file','Start the debugger for the relation Name/Arity which is defined in file File. It is assumed that a predicate name only occurs in the program with the same arity. This prototype debugger implements the framework described in ICLP 2012 paper').
command(30,30,c,debug_sql,'View [Opts]','Debug an SQL view','Debug an SQL view where:\n           Opts=[trust_tables([yes|no])] [trust_file(FileName)].\n           Defaults are trust tables and no trust file').
command(30,40,c,trace_datalog,'Goal [Order]','Trace a Datalog basic goal','Trace a Datalog goal in the given order (postorder or the default preorder)').
command(30,50,c,trace_sql,'View [Order]','Trace an SQL view in preorder (default) or postorder','Trace an SQL view in the given order (postorder or the default preorder)').
command(30,60,c,test_case,'View [Opts]','Generate test case classes for the given view','Generate test case classes for the view View. Options may include a class and/or an action parameters. The test case class is indicated by the values all (positive-negative, the default), positive, or negative in the class parameter. The action is indicated by the values display (only display tuples, the default), replace (replace contents of the involved tables by the computed test case), or add (add the computed test case to the contents of the involved tables) in the action parameter').
command(30,70,c,tc_size,'','Display the test case size bounds','Display the minimum and maximum number of tuples generated for a test case').
command(30,80,c,tc_size,'Min Max','Bound the test case size','Set the minimum and maximum number of tuples generated for a test case').
command(30,90,c,tc_domain,'','Display the domain of values for test cases','Display the domain of values for test cases').
command(30,100,c,tc_domain,'Min Max','Set the domain of values for test cases','Set the domain of values for test cases between Min and Max').

% Tabling:
command(40,10,c,clear_et,'','Clear the extension table','Delete the contents of the extension table').
command(40,20,c,list_et,'','List extension table contents','List the contents of the extension table in lexicographical order. First, answers are displayed, then calls. TAPI enabled').
command(40,30,c,list_et,'Name','List extension table contents matching a name','List the contents of the extension table matching Name. First, answers are displayed, then calls. TAPI enabled').
command(40,40,c,list_et,'Name/Arity','List extension table contents matching the pattern','List the contents of the extension table matching the pattern Name/Arity. First, answers are displayed, then calls. TAPI enabled').

% Operating System:
command(50,05,c,ashell,'Command','Asynchronous submit of Command to the OS shell','Asynchronous submit of Command to the operating system shell. As  /shell Command but without waiting for the process to finish and also eliding output. See also /shell.').
command(50,10,c,cat,'Filename','Type the contents of Filename','Type the contents of Filename enclosed between the following lines: %% BEGIN AbsoluteFilename %% %% END   AbsoluteFilename %%').
command(50,20,c,cd,'','Set current directory to the one DES was started from','Set the current directory to the directory where DES was started from').
command(50,30,c,cd,'Path','Set the current directory to Path','Set the current directory to Path').
command(50,35,y(rm,'Filename'),'del','Filename','Synonym for /rm Filename','Synonym for /rm Filename').
command(50,37,y(edit,'Filename'),'e','Filename','Synonym for /edit Filename','Synonym for /edit Filename').
command(50,40,c,edit,'Filename','Edit Filename','Edit Filename by calling the predefined external text editor. This editor is set with the command /set_editor <your_editor>').
command(50,44,y(ls,''),'dir','','Synonym for /ls','Synonym for /ls').
command(50,46,y(ls,'Path'),'dir','Path','Synonym for /ls Path','Synonym for /ls Path').
command(50,50,c,ls,'','Display the contents of the current directory','Display the contents of the current directory in alphabetical order. First, files are displayed, then directories').
command(50,60,c,ls,'Path','Display the contents of the given directory','Display the contents of the given directory in alphabetical order. It behaves as /ls').
command(50,70,c,pwd,'','Display the current directory','Display the absolute filename for the current directory').
command(50,80,c,rm,'Filename','Delete Filename','Delete (remove) Filename').
command(50,90,c,set_editor,'','Display the current external text editor','Display the current external text editor').
command(50,100,c,set_editor,'Editor','Set the current external text editor to Editor','Set the current external text editor to Editor. This editor is called from the command /edit Filename').
command(50,110,c,shell,'Command','Submit Command to the OS shell','Submit Command to the operating system shell\nNotes for platform specific issues:\n* Windows users:\n  command.exe is the shell for Windows 98, whereas cmd.exe is the one for Windows NT/2000/2003/XP/Vista/7.\n* Ciao users:\n  The environment variable SHELL must be set to the required shell.\n* SICStus users:\n  Under Windows, if the environment variable SHELL is defined, it is expected to name a Unix like shell, which will be invoked with the option -c Command. If SHELL is not defined, the shell named by COMSPEC will be invoked with the option /C Command.\n* Windows and Linux/Unix executable users:\n  The same note for SICStus is applied.').
command(50,120,s(shell,'Command'),'s','Command','Shorthand for /shell','Shorthand for /shell').
command(50,130,y(cat,'Filename'),'type','Filename','Synonym for /cat Filename','Synonym for /cat Filename').

% Logging:
command(60,10,c,log,'','Display the current log file, if any','Display the current log file, if any').
command(60,20,c,log,'Filename','Set the current log to the given filename','Set the current log to the given filename overwriting the file, if exists, or creating a new one').
command(60,30,c,log,'Mode Filename','Set the current log to the given filename and mode','Set the current log to the given filename and mode: write (overwriting the file, if exists, or creating a new one) or append (appending to the contents of the existing file, if exists, or creating a new one)').
command(60,40,c,nolog,'','Disable logging','Disable logging').

% Informative:
command(70,05,y(help,'Keyword'),'apropos','Keyword','Synonym for /help Keyword','Synonym for /help Keyword').
command(70,10,c,builtins,'','List predefined operators, functions, and predicates','List predefined operators, functions, and predicates').
command(70,20,c,dbschema,'','Display the database schema','Display the database schema: Tables, views and constraints. TAPI enabled').
command(70,30,c,dbschema,'Name','Display the database schema for the given connection, view or table','Display the database schema for the given connection, view or table name. TAPI enabled').
command(70,40,c,dbschema,'Connection:Relation','Display the database schema for the given view or table','Display the database schema for the given view or table name. TAPI enabled').
command(70,42,y(dbschema,''),'db_schema','','Synonym for /dbschema','Synonym for /dbschema').
command(70,44,y(dbschema,'Name'),'db_schema','Name','Synonym for /dbschema Name','Synonym for /dbschema Name').
command(70,45,y(dbschema,'Connection:Relation'),'db_schema','Connection:Relation','Synonym for /dbschema Connection:Relation','Synonym for /dbschema Connection:Relation').
command(70,50,c,dependent_relations,'Name','Display relations that directly depend on relation Name','Display the names of relations that directly depend on relation Name. TAPI enabled').
command(70,60,c,dependent_relations,'Name/Arity','Display relations that directly depend on relation Name/Arity','Display in format Name/Arity those relations that directly depend on relation Name/Arity. TAPI enabled').
command(70,70,c,development,'','Display whether development listings are enabled','Display whether development listings are enabled').
command(70,80,c,development,'Switch','Enable or disable development listings','Enable or disable development listings (on or off, resp.) These listings show the source-to-source translations needed to handle null values, Datalog outer join built-ins, and disjunctive literals').
command(70,90,c,display_answer,'','Display whether display of computed tuples is enabled','Display whether display of computed tuples is enabled').
command(70,100,c,display_answer,'Switch','Enable or disable display of computed tuples','Enable or disable display of computed tuples (on or off, resp.) The number of tuples is still displayed if enabled (see the command display_nbr_of_tuples)').
command(70,110,c,display_nbr_of_tuples,'','Display whether display of the number of computed tuples is enabled','Display whether display of the number of computed tuples is enabled').
command(70,120,c,display_nbr_of_tuples,'Switch','Enable or disable display of the number of computed tuples','Enable or disable display of the number of computed tuples (on or off, resp.)').
command(70,130,c,help,'','Display this help','Display detailed help').
command(70,135,s(help,''),'h','','Shorthand for /help','Shorthand for /help').
command(70,140,c,help,'Keyword','Detailed help on Keyword','Detailed help on Keyword, which can be a command or built-in').
command(70,145,s(help,'Keyword'),'h','Keyword','Shorthand for /help Keyword','Shorthand for /help Keyword').
command(70,150,c,is_empty,'Name','Display whether the given relation is empty','Display $true if the given relation is empty, and $false otherwise. TAPI enabled').
command(70,160,c,license,'','Display GPL and LGPL licenses','Display GPL and LGPL licenses. If not found, please visit http://www.gnu.org/licenses').
command(70,170,c,list_table_schemas,'','List table schemas','List table schemas. TAPI enabled').
command(70,180,c,list_table_constraints,'Name','List table constraints for the given table name','List table constraints for the given table name. TAPI enabled').
command(70,190,c,list_tables,'','List table names','List table names. TAPI enabled').
command(70,200,c,list_view_schemas,'','List view schemas','List view schemas. TAPI enabled').
command(70,210,c,list_views,'','List views','List view schemas. TAPI enabled').
command(70,220,c,pdg,'','Display the current predicate dependency graph','Display the current predicate dependency graph').
command(70,225,c,pdg,'Name','Display the predicate dependency graph restricted to predicate with name Name','Display the current predicate dependency graph restricted to predicate with name Name').
command(70,230,c,pdg,'Name/Arity','Display the predicate dependency graph restricted to the predicate Name/Arity','Display the current predicate dependency graph restricted to the predicate Name/Arity').
command(70,235,c,rdg,'','Display the current relation dependency graph','Display the current relation dependency graph').
command(70,240,c,rdg,'Name','Display the relation dependency graph restricted to relation Name','Display the current relation dependency graph restricted to relation with name Name').
command(70,245,c,rdg,'Name/Arity','Display the relation dependency graph restricted to the relation Name/Arity','Display the current relation dependency graph restricted to the relation Name/Arity').
command(70,250,c,referenced_relations,'Name','Display relations directly referenced by a foreign key in Name','Display the name of relations that are directly referenced by a foreign key in relation Name. TAPI enabled').
command(70,260,c,referenced_relations,'Name/Arity','Display relations directly referenced by a foreign key in Name/Arity','Display the name of relations that are directly referenced by a foreign key in relation Name/Arity. TAPI enabled').
command(70,270,c,relation_exists,'Name','Display whether relation Name exists','Display $true if the given relation exists, and $false otherwise. TAPI enabled').
command(70,280,c,relation_schema,'Name','Display relation schema of relation Name','Display relation schema of relation Name. TAPI enabled').
command(70,290,c,prolog_system,'','Display the underlying Prolog engine version','Display the underlying Prolog engine version').
command(70,300,c,sql_left_delimiter,'','Display the SQL left delimiter of current DBMS','Display the SQL left delimiter as defined by the current database manager (either DES or the external DBMS via ODBC). TAPI enabled').
command(70,310,c,sql_right_delimiter,'','Display the SQL right delimiter of current DBMS','Display the SQL right delimiter as defined by the current database manager (either DES or the external DBMS via ODBC). TAPI enabled').
command(70,320,c,status,'','Display the current status of the system','Display the current system status, i.e., verbose mode, logging, elapsed time display, program transformation, current directory, current database and other settings').
command(70,325,c,strata,'','Display the current stratification','Display the current stratification as a list of pairs (PredName/Arity, Stratum)').
command(70,330,c,strata,'Name','Display the current stratification restricted to predicate with name Name','Display the current stratification restricted to predicate with name Name').
command(70,335,c,strata,'Name/Arity','Display the current stratification restricted to the predicate Name/Arity','Display the current stratification restricted to the predicate Name/Arity').
command(70,340,c,verbose,'','Display whether verbose output is enabled','Display whether verbose output is either enabled or disabled (on or off, resp.)').
command(70,350,c,verbose,'Switch','Enable or disable verbose output messages','Enable or disable verbose output messages (on or off, resp.)').
command(70,360,c,version,'','Display the current DES system version','Display the current DES system version').

% Query Languages:
command(80,10,c,datalog,'','Switch to Datalog interpreter','Switch to Datalog interpreter (all queries are parsed and executed first by Datalog engine. If it is not a Datalog query, then it is tried first as an SQL statement. If it is neither SQL, finally it is tried as an RA expression)').
command(80,20,c,datalog,'Query','Trigger Datalog evaluation for Query','Trigger Datalog resolution for the query Query (the query is parsed and executed in Datalog, but if a parsing error is found, it is tried first as an SQL statement and second as an RA expression)').
command(80,30,c,prolog,'','Switch to Prolog interpreter','Switch to Prolog interpreter (all queries are parsed and executed in Prolog)').
command(80,40,c,prolog,'Goal','Trigger Prolog evaluation for Goal','Trigger Prolog\'s SLD resolution for the goal Goal.').
command(80,50,c,ra,'','Switch to RA interpreter','Switch to RA interpreter (all queries are parsed and executed by RA processor)').
command(80,60,c,ra,'RA_expression','Trigger RA evaluation for RA_expression','Trigger RA evaluation for RA_expression').
command(80,70,c,sql,'','Switch to SQL interpreter','Switch to SQL interpreter (all queries are parsed and executed by SQL processor)').
command(80,80,c,sql,'SQL_statement','Trigger SQL evaluation for SQL_statement','Trigger SQL resolution for SQL_statement').

% TAPI:
command(90,10,c,tapi,'Input','Process Input and format its output for TAPI communication','Process Input (either a command or query) and format its output for TAPI communication').
command(90,20,c,test_tapi,'','Test the current TAPI connection','Test the current TAPI connection. Return $success upon a successful communication. TAPI enabled').

% Settings:
command(100,10,c,check,'','Display whether integrity constraint checking is enabled','Display whether integrity constraint checking is enabled').
command(100,20,c,check,'Switch','Enable or disable integrity constraint checking','Enable or disable integrity constraint checking (on or off, resp.)').
command(100,30,c,compact_listings,'','Display whether compact listings are enabled','Display whether compact listings are enabled (on) or not (off)').
command(100,40,c,compact_listings,'Switch','Enable or disable compact listings','Enable or disable compact listings (on or off, resp.)').
command(100,50,c,des_sql_solving,'','Display whether DES is forced to solve SQL queries for external DBs','Display whether DES is forced to solve SQL queries for external DBs. If enabled, this allows to experiment with more expressive queries as, e.g., hypothetical and non-linear recursive queries targeted at an external DBMS').
command(100,55,c,des_sql_solving,'Switch','Enable or disable DES SQL solving for external DBs','Enable or disable DES solving for SQL queries when the current database is an open ODBC connection (on or off, resp.) This allows to experiment with more expressive queries as, e.g., hypothetical and non-linear recursive queries targeted at an external DBMS').
command(100,60,c,display_banner,'','Display whether the banner is displayed at startup','Display whether the system banner is displayed at startup').
command(100,65,c,display_banner,'Switch','Enable or disable the display of the banner at startup','Enable or disable the display of the system banner at startup (on or off, resp.) Only useful in a batch file des.ini or des.cnf').
command(100,70,c,duplicates,'','Display whether duplicates are enabled','Display whether duplicates are enabled').
command(100,75,c,duplicates,'Switch','Enable or disable duplicates','Enable or disable duplicates (on or off, resp.)').
command(100,80,c,fp_info,'','Display whether fixpoint information is to be displayed','Display whether fixpoint information, as the ET entries deduced for the current iteration, is to be displayed').
command(100,85,c,fp_info,'Switch','Enable display of fixpoint information','Enable or disable display of fixpoint information, as the ET entries deduced for the current iteration (on or off, resp.)').
command(100,90,c,hypothetical,'','Display whether whether hypothetical SQL queries are enabled','Display whether hypothetical SQL queries are enabled (on) or not (off)').
command(100,100,c,hypothetical,'Switch','Enable or disable SQL hypothetical queries','Enable or disable hypothetical SQL queries (on or off, resp.)').
command(100,110,c,multiline,'','Display whether multi-line input is enabled','Display whether multi-line input is enabled').
command(100,120,c,multiline,'Switch','Enable or disable multi-line input','Enable or disable  multi-line input (on or off resp.) When enabled, Datalog inputs must end with a dot (.) and SQL inputs with a semicolon (;). When disabled, each line is considered as a single (Datalog or SQL) input and ending characters are optional').
command(100,130,c,nulls,'','Display whether nulls are enabled','Display whether nulls are enabled (on or off, resp.)').
command(100,140,c,nulls,'Switch','Enable or disable nulls','Enable or disable nulls (on or off, resp.)').
command(100,150,c,order_answer,'','Display whether displayed answers are ordered by default','Display whether displayed answers are ordered by default').
command(100,160,c,order_answer,'Switch','Enable or disable a default ordering of displayed computed tuples','Enable or disable a default (ascending) ordering of displayed computed tuples (on or off, resp.) This order is overrided if the user query contains either a group by specification or a call to a view with such a specification').
command(100,170,c,output,'','Display whether display output is enabled','Display whether display output is enabled (on) or not (off)').
command(100,180,c,output,'Switch','Enable or disable display output','Enable or disable display output (on or off, resp.)').
command(100,190,c,pretty_print,'','Display whether pretty print listings is enabled','Display whether pretty print listings is enabled').
command(100,200,c,pretty_print,'Switch','Enable or disable pretty print','Enable or disable pretty print for listings (on or off, resp.)').
command(100,210,c,prompt,'','Display the current value for prompt format','Display the current value for prompt format (des, des_db or plain)').
command(100,220,c,prompt,'Option','Set the format of the prompt (des, des_db, plain, prolog or no)','Set the format of the prompt. The value \'des\' sets the prompt to \'DES>\'. The value \'des_db\' adds the current database name DB as \'DES:DB>\'. The value \'plain\' sets the prompt to \'>\'. The value \'prolog\' sets the prompt to \'?-\'. Finally, \'no\' display nothing for the prompt. Note that, for the values \'des\' and \'des_db\', if a language other than Datalog is selected, the language name preceded by a slash is also displayed before \'>\', as \'DES-SQL>\'').
command(100,224,c,reorder_goals,'','Display whether pushing equalities to the left is enabled','Display whether pushing equalities to the left is enabled').
command(100,225,c,reorder_goals,'Switch','Enable or disable pushing equalities to the left ','Enable or disable pushing equalities to the left (on or off, resp.) Equalities in bodies are moved to the left, which in general allows more efficent computations').
command(100,230,y(restore_default_status,''),'reset','','Synonym for /restore_default_status','Synonym for /restore_default_status').
command(100,235,c,restore_default_status,'','Restore the status of the system to the initial status','Restore the status of the system to the initial status, i.e., set all user-configurable flags to their initial values, including the default database and the start-up directory').
command(100,240,c,running_info,'','Display whether running information is to be displayed','Display whether running information, as the incremental number of consulted rules as they are read, is to be displayed').
command(100,250,c,running_info,'Switch','Enable display of running information','Enable or disable display of running information, as the incremental number of consulted rules as they are read (on or off, resp.)').
command(100,260,c,safe,'','Display whether program transformation for unsafe rules is enabled','Display whether program transformation for unsafe rules is enabled').
command(100,270,c,safe,'Switch','Enable or disable safety transformation','Enable or disable program transformation (on or off, resp.)').
command(100,280,c,safety_warnings,'','Display whether singleton warnings are enabled','Display whether singleton warnings are enabled').
command(100,290,c,safety_warnings,'Switch','Enable or disable singleton warnings','Enable or disable singleton warnings (on or off, resp.)').
command(100,300,c,show_compilations,'','Display whether compilations are to be displayed','Display whether compilations from SQL DQL statements to Datalog rules are to be displayed').
command(100,310,c,show_compilations,'Switch','Enable display of compilations','Enable or disable display of extended information about compilation of SQL DQL statements to Datalog clauses (on or off, resp.)').
command(100,320,c,show_sql,'','Display whether SQL compilations is to be displayed','Display whether SQL compilations are to be displayed').
command(100,330,c,show_sql,'Switch','Enable or disable display of SQL compilations','Enable or disable display of SQL compilations (on or off, resp.) SQL sentences can come from either RA or Datalog compilations. In this last case, they are externally processed').
command(100,340,c,simplification,'','Display whether program simplification is enabled','Display whether program simplification is enabled').
command(100,350,c,simplification,'Switch','Enable or disable program simplification','Enable or disable program simplification (on or off, resp.) Rules with equalities, true, and not(BooleanValue) are simplified. Simplification is always forced for SQL and RA compilations, irrespective of this setting').
command(100,360,c,singleton_warnings,'','Display whether singleton warnings are enabled','Display whether singleton warnings are enabled').
command(100,370,c,singleton_warnings,'Switch','Enable or disable singleton warnings','Enable or disable singleton warnings (on or off, resp.)').
command(100,380,c,type_casting,'','Display whether automatic type casting is enabled','Display whether automatic type casting is enabled').
command(100,390,c,type_casting,'Switch','Enable or disable automatic type casting','Enable or disable automatic type casting (on or off, resp.) This applies to Datalog fact assertions and SQL insertions and selections. Enabling this provides a closer behaviour of SQL statement solving').
command(100,400,c,undef_pred_warnings,'','Display whether undefined predicate warnings are enabled','Display whether undefined predicate warnings are enabled').
command(100,410,c,undef_pred_warnings,'Switch','Enable or disable undefined predicate warnings','Enable or disable undefined predicate warnings (on or off, resp.)').
command(100,420,c,unfold,'','Display whether program unfolding is enabled','Display whether program unfolding is enabled').
command(100,430,c,unfold,'Switch','Enable or disable program unfolding','Enable or disable program unfolding (on or off, resp.) Unfolding affects to the set of rules which result from the compilation of a single source rule. Unfolding is always forced for SQL and RA compilations, irrespective of this setting').

% Timing:
command(110,10,c,display_stopwatch,'','Display stopwatch','Display stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(110,20,c,format_timing,'','Display whether formatted timing is enabled','Display whether formatted timing is enabled').
command(110,30,c,format_timing,'Switch','Enable or disable formatted timing','Enable or disable formatted timing (on or off, resp.) Given that ms, s, m, h represent milliseconds, seconds, minutes, and hours, respectively, times less than 1 second are displayed as ms; times between 1 second and less than 60 are displayed as s.ms; times between 60 seconds and less than 60 minutes are displayed as m:s.ms; and times from 60 minutes on are displayed as h:m:s.ms').
command(110,40,c,reset_stopwatch,'','Reset stopwatch','Reset stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(110,50,c,start_stopwatch,'','Start stopwatch','Start stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(110,60,c,stop_stopwatch,'','Stop stopwatch','Stop stopwatch. Precision depends on host Prolog system (1 second or milliseconds)').
command(110,70,c,timing,'','Display whether elapsed time display is enabled','Display whether elapsed time display is enabled').
command(110,80,c,timing,'Option','Sets the required level of elapsed time display','Sets the required level of elapsed time display as disabled, enabled, or detailed (off, on, detailed, resp.)').

% Statistics:
command(120,10,c,host_statistics,'Keyword','Display host Prolog statistics for Keyword (\'runtime\' or \'total_runtime\')','Display host Prolog statistics for Keyword (\'runtime\' or \'total_runtime\'). For \'runtime\', this command displays the CPU time used while executing, excluding time spent in memory management tasks or in system calls since the last call to this command. For \'total_runtime\', this command displays the total CPU time used while executing, including memory management tasks such as garbage collection but excluding system calls since the last call to this command.').
command(120,20,c,statistics,'','Display whether statistics collection is enabled','Display whether statistics collection is enabled or not (on or off, resp.) It also displays last statistics, if enabled').
command(120,30,c,statistics,'Switch','Enable or disable statistics collection ','Enable or disable statistics collection (on or off, resp., and disabled by default). Statistics include numbers for: Fixpoint iterations, EDB and IDB retrievals, ET retrievals, and ET (Extension Table), CT (Call Table) and CF (complete computations) lookups').

% Miscellanea:
command(130,00,y(halt,''),'exit','','Synonym for /halt','Synonym for /halt').
command(130,05,s(exit,''),'e','','Shorthand for /exit','Shorthand for /exit').
command(130,10,c,halt,'','Quit DES','Quit DES. The Prolog host is also exited').
command(130,20,c,process,'Filename [Parameters]','Process the contents of Filename','Process the contents of Filename as if they were typed at the system prompt. A parameter is a string delimited by either blanks or double quotes (") if the parameter contains a blank. The same is applied to Filename. The value for each parameter is retrieved by the tokens $parv1$, $parv2$, ... for the first, second, ... parameter, respectively').
command(130,30,s(process,'Filename [Parameters]'),'p','Filename','Shorthand for /process Filename [Parameters]','Shorthand for /process Filename [Parameters]').
command(130,40,y(halt,''),'quit','','Synonym for /halt','Synonym for /halt').
command(130,50,s(quit,''),'q','','Shorthand for /quit','Shorthand for /quit').
command(130,60,c,repeat,'Number Input','Repeat Input as many times as Number','Repeat Input as many times as Number, where Input can be any legal input at the command prompt').

% Implementors:
command(140,10,c,debug,'','Enable debugging in the host Prolog interpreter','Enable debugging in the host Prolog interpreter. Only working for source distributions').
command(140,20,c,indexing,'','Display whether hash indexing is enabled','Display whether hash indexing on memo tables is enabled').
command(140,30,c,indexing,'Switch','Enable or disable hash indexing','Enable or disable hash indexing on extension table (on or off, resp.) Default is enabled, which shows a noticeable speed-up gain in some cases').
command(140,40,c,nospyall,'','Remove all Prolog spy points','Remove all Prolog spy points in the host Prolog interpreter. Disable debugging. Only working for source distributions').
command(140,50,c,nospy,'Pred[/Arity]','Remove the spy point on the given predicate','Remove the spy point on the given predicate in the host Prolog interpreter. Only working for source distributions').
command(140,60,c,optimize_cc,'','Display whether complete computations optimization is enabled','Display whether complete computations optimization is enabled or not (on or off, resp.)').
command(140,70,c,optimize_cc,'Switch','Enable or disable complete computations optimization','Enable or disable complete computations optimization (on or off, resp. and enabled by default). Fixpoint iterations and/or extensional database retrievals might been saved').
command(140,80,c,optimize_ep,'','Display whether extensional predicates optimization is enabled','Display whether extensional predicates optimization is enabled or not (on or off, resp. and enabled by default)').
command(140,90,c,optimize_ep,'Switch','Enable or disable extensional predicates optimization','Enable or disable extensional predicates optimization (on or off, resp. and enabled by default). Fixpoint iterations and extensional database retrievals are saved for extensional predicates as a single linear fetching is performed for computing them').
command(140,100,c,optimize_nrp,'','Display whether non-recursive predicates optimization is enabled','Display whether non-recursive predicates optimization is enabled or not (on or off, resp.)').
command(140,110,c,optimize_nrp,'Switch','Enable or disable non-recursive predicates optimization','Enable or disable non-recursive predicates optimization (on or off, resp. and enabled by default). Memoing is only performed for top-level goals').
command(140,120,c,optimize_st,'','Display whether stratum optimization is enabled','Display whether stratum optimization is enabled or not (on or off, resp. and disabled by default)').
command(140,130,c,optimize_st,'Switch','Enable or disable stratum optimization','Enable or disable stratum optimization (on or off, resp. and enabled by default). Extensional table lookups are saved for non-recursive predicates calling to recursive ones, but more tuples might be computed if the non-recursive call is filtered, as in this case an open call is submitted instead (i.e., not filtered)').
command(140,140,c,spy,'Pred[/Arity]','Set a spy point on the given predicate','Set a spy point on the given predicate in the host Prolog interpreter. Binary distributions do not support spy points. Use source distributions instead').
command(140,150,c,system,'Goal','Submit a goal to the host system','Submit a goal to the host Prolog system').
command(140,160,c,terminate,'','Terminate the current DES session','Terminate the current DES session without halting the host Prolog system').
command(140,165,s(terminate,''),'t','','Shorthand for /terminate','Shorthand for /terminate').
command(140,170,c,write,'String','Write String to console','Write String to console. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').
command(140,180,c,writeln,'String','Write String to console appending a new line','Write String to File appending a new line. If File does not exist, it is created; otherwise, previous contents are not deleted and String is simply appended to File. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after the command is considered as the string').
command(140,190,c,write_to_file,'File String','Write String to File','Write String to File. If File does not exist, it is created; otherwise, previous contents are not deleted and String is simply appended to File. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time). Strings are not needed to be delimited: the text after File is considered as the string').
command(140,200,c,writeln_to_file,'File','Write a new line to File','Appends a new line to File. If File does not exist, it is created; otherwise, previous contents are not deleted and the new line is simply appended to File. String can contain system variables as $stopwatch$ (which holds the current stopwatch time) and $total_elapsed_time$ (which holds the last total elapsed time)').


% LogiQL:
command(150,10,c,logiql,'','Display whether LoqiQL output is enabled','Display whether LoqiQL output is enabled').
command(150,20,c,logiql,'Switch','Enable or disable LoqiQL output','Enable or disable LoqiQL output (on or off, resp.)').
command(150,30,s(logiql,''),'lq','','Shorthand for /logiql','Shorthand for /logiql').
command(150,40,s(logiql,'Switch'),'lq','','Shorthand for /logiql Switch','Shorthand for /logiql Switch').
command(150,50,c,logiql_log,'','Display the current LogiQL log file, if any','Display the current LogiQL log file, if any').
command(150,60,c,logiql_log,'Filename','Set the current LogiQL log to the given filename','Set the current LogiQL log to the given filename overwriting the file, if exists, or creating a new one').
command(150,70,c,logiql_log,'Mode Filename','Set the current LogiQL log to the given filename and mode','Set the current LogiQL log to the given filename and mode: write (overwriting the file, if exists, or creating a new one) or append (appending to the contents of the existing file, if exists, or creating a new one)').
command(150,80,c,logiql_nolog,'','Disable LogiQL logging','Disable LogiQL logging').


% ASSIGN NUMBERS TO COMMANDS:
% Run in SWI-Prolog
% Uncomment the following code, 
% run write_commands_new_ids, which generates cmds.pl, 
% and paste its contents above, replacing old command predicate
% write_commands_new_ids :-
%   setof(CategoryId-Category-Commands,
%         (command_category(CategoryId,Category),
%          setof(command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc), 
%                command(CategoryId,CommandId,Type,Command,Arguments,ShortDesc,ExtDesc),
%                Commands)),
%         IdsCategoriesCommands),
%   tell('cmds.pl'),
%   write_commands(IdsCategoriesCommands),
%   told.

% write_commands([]).
% write_commands([_CategoryId-Category-Commands|Tail]) :-
%   write_log_list([nl, '% ',Category,':',nl]),
%   write_commands_list(Commands,10),
%   write_commands(Tail).

% write_commands_list([],_).
% write_commands_list([command(C,_N,A3,A4,A5,A6,A7)|Cmds],ID) :-
%   write_term(command(C,ID,A3,A4,A5,A6,A7),[character_escapes(true),quoted(true)]),
%   write('.'),
%   nl,
%   ID1 is ID+10,
%   write_commands_list(Cmds,ID1).