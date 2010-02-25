package oomapping;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.framework.Assert;

import java.util.List;

import base.hand.types.*;
import base.hand.types.t1.*;
import base.hand.types.t2.*;
import tom.library.sl.*;
import oomapping.signature.ISignature;

public class OOMappingTest extends TestCase {
  
  private static boolean tom_equal_term_Strategy(Object t1, Object t2) {return  (t1.equals(t2)) ;}private static boolean tom_is_sort_Strategy(Object t) {return  (t instanceof tom.library.sl.Strategy) ;} private static boolean tom_equal_term_Position(Object t1, Object t2) {return  (t1.equals(t2)) ;}private static boolean tom_is_sort_Position(Object t) {return  (t instanceof tom.library.sl.Position) ;} private static boolean tom_equal_term_int(int t1, int t2) {return  t1==t2 ;}private static boolean tom_is_sort_int(int t) {return  true ;} private static boolean tom_equal_term_char(char t1, char t2) {return  t1==t2 ;}private static boolean tom_is_sort_char(char t) {return  true ;} private static boolean tom_equal_term_String(String t1, String t2) {return  t1.equals(t2) ;}private static boolean tom_is_sort_String(String t) {return  t instanceof String ;} private static  tom.library.sl.Strategy  tom_make_mu( tom.library.sl.Strategy  var,  tom.library.sl.Strategy  v) { return ( new tom.library.sl.Mu(var,v) );}private static  tom.library.sl.Strategy  tom_make_MuVar( String  name) { return ( new tom.library.sl.MuVar(name) );}private static  tom.library.sl.Strategy  tom_make_Identity() { return ( new tom.library.sl.Identity() );}private static  tom.library.sl.Strategy  tom_make_One( tom.library.sl.Strategy  v) { return ( new tom.library.sl.One(v) );}private static  tom.library.sl.Strategy  tom_make_All( tom.library.sl.Strategy  v) { return ( new tom.library.sl.All(v) );}private static  tom.library.sl.Strategy  tom_make_Fail() { return ( new tom.library.sl.Fail() );}private static boolean tom_is_fun_sym_Sequence( tom.library.sl.Strategy  t) {return ( (t instanceof tom.library.sl.Sequence) );}private static  tom.library.sl.Strategy  tom_empty_list_Sequence() { return ( null );}private static  tom.library.sl.Strategy  tom_cons_list_Sequence( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { return ( (tail==null)?head:new tom.library.sl.Sequence(head,tail) );}private static  tom.library.sl.Strategy  tom_get_head_Sequence_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Sequence.FIRST) );}private static  tom.library.sl.Strategy  tom_get_tail_Sequence_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Sequence.THEN) );}private static boolean tom_is_empty_Sequence_Strategy( tom.library.sl.Strategy  t) {return ( t == null );}   private static   tom.library.sl.Strategy  tom_append_list_Sequence( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {     if(( l1 == null )) {       return l2;     } else if(( l2 == null )) {       return l1;     } else if(( (l1 instanceof tom.library.sl.Sequence) )) {       if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ) == null )) {         return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ):new tom.library.sl.Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ),l2) );       } else {         return ( (tom_append_list_Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ):new tom.library.sl.Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ),tom_append_list_Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ),l2)) );       }     } else {       return ( (l2==null)?l1:new tom.library.sl.Sequence(l1,l2) );     }   }   private static   tom.library.sl.Strategy  tom_get_slice_Sequence( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {     if( (begin.equals(end)) ) {       return tail;     } else if( (end.equals(tail))  && (( end == null ) ||  (end.equals(tom_empty_list_Sequence())) )) {       /* code to avoid a call to make, and thus to avoid looping during list-matching */       return begin;     }     return ( (( tom.library.sl.Strategy )tom_get_slice_Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.THEN) ):tom_empty_list_Sequence()),end,tail)==null)?((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.FIRST) ):begin):new tom.library.sl.Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.THEN) ):tom_empty_list_Sequence()),end,tail)) );   }   private static boolean tom_is_fun_sym_Choice( tom.library.sl.Strategy  t) {return ( (t instanceof tom.library.sl.Choice) );}private static  tom.library.sl.Strategy  tom_empty_list_Choice() { return ( null );}private static  tom.library.sl.Strategy  tom_cons_list_Choice( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { return ( (tail==null)?head:new tom.library.sl.Choice(head,tail) );}private static  tom.library.sl.Strategy  tom_get_head_Choice_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Choice.FIRST) );}private static  tom.library.sl.Strategy  tom_get_tail_Choice_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Choice.THEN) );}private static boolean tom_is_empty_Choice_Strategy( tom.library.sl.Strategy  t) {return ( t ==null );}   private static   tom.library.sl.Strategy  tom_append_list_Choice( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {     if(( l1 ==null )) {       return l2;     } else if(( l2 ==null )) {       return l1;     } else if(( (l1 instanceof tom.library.sl.Choice) )) {       if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ) ==null )) {         return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ):new tom.library.sl.Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ),l2) );       } else {         return ( (tom_append_list_Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ):new tom.library.sl.Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ),tom_append_list_Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ),l2)) );       }     } else {       return ( (l2==null)?l1:new tom.library.sl.Choice(l1,l2) );     }   }   private static   tom.library.sl.Strategy  tom_get_slice_Choice( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {     if( (begin.equals(end)) ) {       return tail;     } else if( (end.equals(tail))  && (( end ==null ) ||  (end.equals(tom_empty_list_Choice())) )) {       /* code to avoid a call to make, and thus to avoid looping during list-matching */       return begin;     }     return ( (( tom.library.sl.Strategy )tom_get_slice_Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.THEN) ):tom_empty_list_Choice()),end,tail)==null)?((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.FIRST) ):begin):new tom.library.sl.Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.THEN) ):tom_empty_list_Choice()),end,tail)) );   }   private static boolean tom_is_fun_sym_SequenceId( tom.library.sl.Strategy  t) {return ( (t instanceof tom.library.sl.SequenceId) );}private static  tom.library.sl.Strategy  tom_empty_list_SequenceId() { return ( null );}private static  tom.library.sl.Strategy  tom_cons_list_SequenceId( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { return ( (tail==null)?head:new tom.library.sl.SequenceId(head,tail) );}private static  tom.library.sl.Strategy  tom_get_head_SequenceId_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.SequenceId.FIRST) );}private static  tom.library.sl.Strategy  tom_get_tail_SequenceId_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.SequenceId.THEN) );}private static boolean tom_is_empty_SequenceId_Strategy( tom.library.sl.Strategy  t) {return ( t == null );}   private static   tom.library.sl.Strategy  tom_append_list_SequenceId( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {     if(( l1 == null )) {       return l2;     } else if(( l2 == null )) {       return l1;     } else if(( (l1 instanceof tom.library.sl.SequenceId) )) {       if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.THEN) ) == null )) {         return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.FIRST) ):new tom.library.sl.SequenceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.FIRST) ),l2) );       } else {         return ( (tom_append_list_SequenceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.FIRST) ):new tom.library.sl.SequenceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.FIRST) ),tom_append_list_SequenceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.SequenceId.THEN) ),l2)) );       }     } else {       return ( (l2==null)?l1:new tom.library.sl.SequenceId(l1,l2) );     }   }   private static   tom.library.sl.Strategy  tom_get_slice_SequenceId( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {     if( (begin.equals(end)) ) {       return tail;     } else if( (end.equals(tail))  && (( end == null ) ||  (end.equals(tom_empty_list_SequenceId())) )) {       /* code to avoid a call to make, and thus to avoid looping during list-matching */       return begin;     }     return ( (( tom.library.sl.Strategy )tom_get_slice_SequenceId(((( (begin instanceof tom.library.sl.SequenceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.SequenceId.THEN) ):tom_empty_list_SequenceId()),end,tail)==null)?((( (begin instanceof tom.library.sl.SequenceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.SequenceId.FIRST) ):begin):new tom.library.sl.SequenceId(((( (begin instanceof tom.library.sl.SequenceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.SequenceId.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_SequenceId(((( (begin instanceof tom.library.sl.SequenceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.SequenceId.THEN) ):tom_empty_list_SequenceId()),end,tail)) );   }   private static boolean tom_is_fun_sym_ChoiceId( tom.library.sl.Strategy  t) {return ( (t instanceof tom.library.sl.ChoiceId) );}private static  tom.library.sl.Strategy  tom_empty_list_ChoiceId() { return ( null );}private static  tom.library.sl.Strategy  tom_cons_list_ChoiceId( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { return ( (tail==null)?head:new tom.library.sl.ChoiceId(head,tail) );}private static  tom.library.sl.Strategy  tom_get_head_ChoiceId_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.ChoiceId.FIRST) );}private static  tom.library.sl.Strategy  tom_get_tail_ChoiceId_Strategy( tom.library.sl.Strategy  t) {return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.ChoiceId.THEN) );}private static boolean tom_is_empty_ChoiceId_Strategy( tom.library.sl.Strategy  t) {return ( t ==null );}   private static   tom.library.sl.Strategy  tom_append_list_ChoiceId( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {     if(( l1 ==null )) {       return l2;     } else if(( l2 ==null )) {       return l1;     } else if(( (l1 instanceof tom.library.sl.ChoiceId) )) {       if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.THEN) ) ==null )) {         return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.FIRST) ):new tom.library.sl.ChoiceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.FIRST) ),l2) );       } else {         return ( (tom_append_list_ChoiceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.FIRST) ):new tom.library.sl.ChoiceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.FIRST) ),tom_append_list_ChoiceId(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.ChoiceId.THEN) ),l2)) );       }     } else {       return ( (l2==null)?l1:new tom.library.sl.ChoiceId(l1,l2) );     }   }   private static   tom.library.sl.Strategy  tom_get_slice_ChoiceId( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {     if( (begin.equals(end)) ) {       return tail;     } else if( (end.equals(tail))  && (( end ==null ) ||  (end.equals(tom_empty_list_ChoiceId())) )) {       /* code to avoid a call to make, and thus to avoid looping during list-matching */       return begin;     }     return ( (( tom.library.sl.Strategy )tom_get_slice_ChoiceId(((( (begin instanceof tom.library.sl.ChoiceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.ChoiceId.THEN) ):tom_empty_list_ChoiceId()),end,tail)==null)?((( (begin instanceof tom.library.sl.ChoiceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.ChoiceId.FIRST) ):begin):new tom.library.sl.ChoiceId(((( (begin instanceof tom.library.sl.ChoiceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.ChoiceId.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_ChoiceId(((( (begin instanceof tom.library.sl.ChoiceId) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.ChoiceId.THEN) ):tom_empty_list_ChoiceId()),end,tail)) );   }   private static  tom.library.sl.Strategy  tom_make_OneId( tom.library.sl.Strategy  v) { return ( new tom.library.sl.OneId(v) );}   private static  tom.library.sl.Strategy  tom_make_AllSeq( tom.library.sl.Strategy  s) { return ( new tom.library.sl.AllSeq(s) );}private static  tom.library.sl.Strategy  tom_make_AUCtl( tom.library.sl.Strategy  s1,  tom.library.sl.Strategy  s2) { return ( tom_make_mu(tom_make_MuVar("x"),tom_cons_list_Choice(s2,tom_cons_list_Choice(tom_cons_list_Sequence(tom_cons_list_Sequence(s1,tom_cons_list_Sequence(tom_make_All(tom_make_MuVar("x")),tom_empty_list_Sequence())),tom_cons_list_Sequence(tom_make_One(tom_make_Identity()),tom_empty_list_Sequence())),tom_empty_list_Choice()))) );}private static  tom.library.sl.Strategy  tom_make_EUCtl( tom.library.sl.Strategy  s1,  tom.library.sl.Strategy  s2) { return ( tom_make_mu(tom_make_MuVar("x"),tom_cons_list_Choice(s2,tom_cons_list_Choice(tom_cons_list_Sequence(s1,tom_cons_list_Sequence(tom_make_One(tom_make_MuVar("x")),tom_empty_list_Sequence())),tom_empty_list_Choice()))));} private static  tom.library.sl.Strategy  tom_make_Try( tom.library.sl.Strategy  v) { return ( tom_cons_list_Choice(v,tom_cons_list_Choice(tom_make_Identity(),tom_empty_list_Choice())) );}private static  tom.library.sl.Strategy  tom_make_TopDown( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_Sequence(v,tom_cons_list_Sequence(tom_make_All(tom_make_MuVar("_x")),tom_empty_list_Sequence()))) );}private static  tom.library.sl.Strategy  tom_make_OnceBottomUp( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_Choice(tom_make_One(tom_make_MuVar("_x")),tom_cons_list_Choice(v,tom_empty_list_Choice()))) );}private static  tom.library.sl.Strategy  tom_make_OnceTopDown( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_Choice(v,tom_cons_list_Choice(tom_make_One(tom_make_MuVar("_x")),tom_empty_list_Choice()))) );}private static  tom.library.sl.Strategy  tom_make_Repeat( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_Choice(tom_cons_list_Sequence(v,tom_cons_list_Sequence(tom_make_MuVar("_x"),tom_empty_list_Sequence())),tom_cons_list_Choice(tom_make_Identity(),tom_empty_list_Choice()))) );}private static  tom.library.sl.Strategy  tom_make_RepeatId( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_SequenceId(v,tom_cons_list_SequenceId(tom_make_MuVar("_x"),tom_empty_list_SequenceId()))) );}private static  tom.library.sl.Strategy  tom_make_OnceTopDownId( tom.library.sl.Strategy  v) { return ( tom_make_mu(tom_make_MuVar("_x"),tom_cons_list_ChoiceId(v,tom_cons_list_ChoiceId(tom_make_OneId(tom_make_MuVar("_x")),tom_empty_list_ChoiceId()))) );}   private static boolean tom_equal_term_ListT1(Object t1, Object t2) {return  (t1 == t2) ;}private static boolean tom_is_sort_ListT1(Object t) {return  (t instanceof ListT1) ;}private static boolean tom_equal_term_T1(Object t1, Object t2) {return  (t1 == t2) ;}private static boolean tom_is_sort_T1(Object t) {return  (t instanceof T1) ;}private static boolean tom_equal_term_T2(Object t1, Object t2) {return  (t1 == t2) ;}private static boolean tom_is_sort_T2(Object t) {return  (t instanceof T2) ;}private static boolean tom_is_fun_sym_a( T1  t) {return getSignature().getMapping_a().isInstanceOf(t) ;}private static  T1  tom_make_a() { return  (getSignature().getMapping_a().make()) ;}private static boolean tom_is_fun_sym_f( T1  t) {return getSignature().getMapping_f().isInstanceOf(t) ;}private static  T1  tom_make_f( T1  s1,  T2  s2) { return  (getSignature().getMapping_f().make(s1,s2)) ;}private static  T1  tom_get_slot_f_s1( T1  t) {return getSignature().getMapping_f().get0(t) ;}private static  T2  tom_get_slot_f_s2( T1  t) {return getSignature().getMapping_f().get1(t) ;}private static  T2  tom_make_b() { return  (getSignature().getMapping_b().make()) ;}private static boolean tom_is_fun_sym_g( T2  t) {return getSignature().getMapping_g().isInstanceOf(t) ;}private static  T2  tom_make_g( T2  s2) { return  (getSignature().getMapping_g().make(s2)) ;}private static  T2  tom_get_slot_g_s2( T2  t) {return getSignature().getMapping_g().get0(t) ;}private static boolean tom_is_fun_sym_h( T2  t) {return getSignature().getMapping_h().isInstanceOf(t) ;}private static  T2  tom_make_h( ListT1  ts) { return  (getSignature().getMapping_h().make(ts)) ;}private static  ListT1  tom_get_slot_h_ts( T2  t) {return getSignature().getMapping_h().get0(t) ;}private static boolean tom_is_fun_sym_concT1( ListT1  l) {return  getSignature().getMapping_concT1().isInstanceOf(l) ;}private static  ListT1  tom_empty_list_concT1() { return  getSignature().getMapping_concT1().getMapping_L().makeEmpty() ;}private static  ListT1  tom_cons_list_concT1( T1  o,  ListT1  l) { return  getSignature().getMapping_concT1().getMapping_L().makeInsert(o,l) ;}private static  T1  tom_get_head_concT1_ListT1( ListT1  l) {return  getSignature().getMapping_concT1().getMapping_L().getHead(l) ;}private static  ListT1  tom_get_tail_concT1_ListT1( ListT1  l) {return  getSignature().getMapping_concT1().getMapping_L().getTail(l) ;}private static boolean tom_is_empty_concT1_ListT1( ListT1  l) {return  getSignature().getMapping_concT1().getMapping_L().isEmpty(l) ;}   private static   ListT1  tom_append_list_concT1( ListT1 l1,  ListT1  l2) {     if( getSignature().getMapping_concT1().getMapping_L().isEmpty(l1) ) {       return l2;     } else if( getSignature().getMapping_concT1().getMapping_L().isEmpty(l2) ) {       return l1;     } else if( getSignature().getMapping_concT1().getMapping_L().isEmpty( getSignature().getMapping_concT1().getMapping_L().getTail(l1) ) ) {       return  getSignature().getMapping_concT1().getMapping_L().makeInsert( getSignature().getMapping_concT1().getMapping_L().getHead(l1) ,l2) ;     } else {       return  getSignature().getMapping_concT1().getMapping_L().makeInsert( getSignature().getMapping_concT1().getMapping_L().getHead(l1) ,tom_append_list_concT1( getSignature().getMapping_concT1().getMapping_L().getTail(l1) ,l2)) ;     }   }   private static   ListT1  tom_get_slice_concT1( ListT1  begin,  ListT1  end, ListT1  tail) {     if( (begin == end) ) {       return tail;     } else if( (end == tail)  && ( getSignature().getMapping_concT1().getMapping_L().isEmpty(end)  ||  (end == tom_empty_list_concT1()) )) {       /* code to avoid a call to make, and thus to avoid looping during list-matching */       return begin;     }     return  getSignature().getMapping_concT1().getMapping_L().makeInsert( getSignature().getMapping_concT1().getMapping_L().getHead(begin) ,( ListT1 )tom_get_slice_concT1( getSignature().getMapping_concT1().getMapping_L().getTail(begin) ,end,tail)) ;   }   

  
  /*
   * the body of the following method is hand-written
   */
  public static ISignature getSignature() {
    return signature;
  }

  /* code by hand */
  private static ISignature signature = new ISignature() {
    private Module.a_Mapping a_Mapping = new Module.a_Mapping();
    private Module.b_Mapping b_Mapping = new Module.b_Mapping();
    private Module.f_Mapping f_Mapping = new Module.f_Mapping();
    private Module.g_Mapping g_Mapping = new Module.g_Mapping();
    private Module.h_Mapping h_Mapping = new Module.h_Mapping();
    private Module.concT1_Mapping concT1_Mapping = new Module.concT1_Mapping();

    public tom.library.oomapping.Mapping0<T1>       getMapping_a() { return a_Mapping; }
    public tom.library.oomapping.Mapping0<T2>       getMapping_b() { return b_Mapping; }
    public tom.library.oomapping.Mapping2<T1,T1,T2> getMapping_f() { return f_Mapping; }
    public tom.library.oomapping.Mapping1<T2,T2>    getMapping_g() { return g_Mapping; }
    public tom.library.oomapping.Mapping1<T2,java.util.List<T1>>    getMapping_h() { return h_Mapping; }
    public tom.library.oomapping.ListMapping<java.util.List<T1>,T1>    getMapping_concT1() { return concT1_Mapping; }
  };


  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(OOMappingTest.class));
  }

  public void testMatch() {
    T1 subject = tom_make_f(tom_make_f(tom_make_a(),tom_make_b()),tom_make_g(tom_make_b()));
    {{if (tom_is_sort_T1(subject)) {if (tom_is_fun_sym_f((( T1 )subject))) { T2  tomMatch1_2=tom_get_slot_f_s2((( T1 )subject));if (tom_is_fun_sym_g(tomMatch1_2)) {
 
        assertEquals(tom_get_slot_f_s1((( T1 )subject)),tom_make_f(tom_make_a(),tom_make_b()));
        assertEquals(tom_get_slot_g_s2(tomMatch1_2),tom_make_b());
        return;
      }}}}}

    fail();
  }

  public void testVisit() {
    T1 subject = tom_make_f(tom_make_f(tom_make_a(),tom_make_b()),tom_make_g(tom_make_b()));
    try {
      T1 res1 = (T1)
        tom_make_Repeat(tom_make_OnceBottomUp(tom_make_Rule())).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
      T1 res2 = (T1)
        tom_make_Repeat(tom_make_OnceBottomUp(tom_make_Rule())).visit(subject,tom.library.oomapping.Introspector.getInstance());
      assertEquals(res1, tom_make_a());
      assertEquals(res1, res2);
    } catch(VisitFailure e) {
      fail();
    }

  }

  public static class Print extends tom.library.sl.AbstractStrategyBasic {public Print() {super(tom_make_Identity());}public tom.library.sl.Visitable[] getChildren() {tom.library.sl.Visitable[] stratChilds = new tom.library.sl.Visitable[getChildCount()];stratChilds[0] = super.getChildAt(0);return stratChilds;}public tom.library.sl.Visitable setChildren(tom.library.sl.Visitable[] children) {super.setChildAt(0, children[0]);return this;}public int getChildCount() {return 1;}public tom.library.sl.Visitable getChildAt(int index) {switch (index) {case 0: return super.getChildAt(0);default: throw new IndexOutOfBoundsException();}}public tom.library.sl.Visitable setChildAt(int index, tom.library.sl.Visitable child) {switch (index) {case 0: return super.setChildAt(0, child);default: throw new IndexOutOfBoundsException();}}@SuppressWarnings("unchecked")public  T1  visit_T1( T1  tom__arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {{{if (tom_is_sort_T1(tom__arg)) {

 System.out.println((( T1 )tom__arg)/*+ " : " + getPosition()*/); }}}return _visit_T1(tom__arg,introspector);}@SuppressWarnings("unchecked")public  T2  visit_T2( T2  tom__arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {{{if (tom_is_sort_T2(tom__arg)) {


 System.out.println((( T2 )tom__arg)/*+ " : " + getPosition()*/); }}}return _visit_T2(tom__arg,introspector);}@SuppressWarnings("unchecked")public  T2  _visit_T2( T2  arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (!(( null  == environment))) {return (( T2 )any.visit(environment,introspector));} else {return any.visitLight(arg,introspector);}}@SuppressWarnings("unchecked")public  T1  _visit_T1( T1  arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (!(( null  == environment))) {return (( T1 )any.visit(environment,introspector));} else {return any.visitLight(arg,introspector);}}@SuppressWarnings("unchecked")public <T> T visitLight(T v, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (tom_is_sort_T2(v)) {return ((T)visit_T2((( T2 )v),introspector));}if (tom_is_sort_T1(v)) {return ((T)visit_T1((( T1 )v),introspector));}if (!(( null  == environment))) {return ((T)any.visit(environment,introspector));} else {return any.visitLight(v,introspector);}}}public static class Rule extends tom.library.sl.AbstractStrategyBasic {public Rule() {super(tom_make_Fail());}public tom.library.sl.Visitable[] getChildren() {tom.library.sl.Visitable[] stratChilds = new tom.library.sl.Visitable[getChildCount()];stratChilds[0] = super.getChildAt(0);return stratChilds;}public tom.library.sl.Visitable setChildren(tom.library.sl.Visitable[] children) {super.setChildAt(0, children[0]);return this;}public int getChildCount() {return 1;}public tom.library.sl.Visitable getChildAt(int index) {switch (index) {case 0: return super.getChildAt(0);default: throw new IndexOutOfBoundsException();}}public tom.library.sl.Visitable setChildAt(int index, tom.library.sl.Visitable child) {switch (index) {case 0: return super.setChildAt(0, child);default: throw new IndexOutOfBoundsException();}}@SuppressWarnings("unchecked")public  T1  visit_T1( T1  tom__arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {{{if (tom_is_sort_T1(tom__arg)) {if (tom_is_fun_sym_f((( T1 )tom__arg))) {






        return tom_get_slot_f_s1((( T1 )tom__arg));
      }}}}return _visit_T1(tom__arg,introspector);}@SuppressWarnings("unchecked")public  T1  _visit_T1( T1  arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (!(( null  == environment))) {return (( T1 )any.visit(environment,introspector));} else {return any.visitLight(arg,introspector);}}@SuppressWarnings("unchecked")public <T> T visitLight(T v, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (tom_is_sort_T1(v)) {return ((T)visit_T1((( T1 )v),introspector));}if (!(( null  == environment))) {return ((T)any.visit(environment,introspector));} else {return any.visitLight(v,introspector);}}}private static  tom.library.sl.Strategy  tom_make_Rule() { return new Rule();}public static class Rule2 extends tom.library.sl.AbstractStrategyBasic {public Rule2() {super(tom_make_Fail());}public tom.library.sl.Visitable[] getChildren() {tom.library.sl.Visitable[] stratChilds = new tom.library.sl.Visitable[getChildCount()];stratChilds[0] = super.getChildAt(0);return stratChilds;}public tom.library.sl.Visitable setChildren(tom.library.sl.Visitable[] children) {super.setChildAt(0, children[0]);return this;}public int getChildCount() {return 1;}public tom.library.sl.Visitable getChildAt(int index) {switch (index) {case 0: return super.getChildAt(0);default: throw new IndexOutOfBoundsException();}}public tom.library.sl.Visitable setChildAt(int index, tom.library.sl.Visitable child) {switch (index) {case 0: return super.setChildAt(0, child);default: throw new IndexOutOfBoundsException();}}@SuppressWarnings("unchecked")public  T2  visit_T2( T2  tom__arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {{{if (tom_is_sort_T2(tom__arg)) {if (tom_is_fun_sym_g((( T2 )tom__arg))) {return tom_get_slot_g_s2((( T2 )tom__arg));}}}}return _visit_T2(tom__arg,introspector);}@SuppressWarnings("unchecked")public  T2  _visit_T2( T2  arg, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (!(( null  == environment))) {return (( T2 )any.visit(environment,introspector));} else {return any.visitLight(arg,introspector);}}@SuppressWarnings("unchecked")public <T> T visitLight(T v, tom.library.sl.Introspector introspector) throws tom.library.sl.VisitFailure {if (tom_is_sort_T2(v)) {return ((T)visit_T2((( T2 )v),introspector));}if (!(( null  == environment))) {return ((T)any.visit(environment,introspector));} else {return any.visitLight(v,introspector);}}}









  public void test_listMatchFirst() {
    List<T1> subject = tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1())));

    {{if (tom_is_sort_ListT1(subject)) {if (tom_is_fun_sym_concT1((( ListT1 )subject))) {if (!(tom_is_empty_concT1_ListT1((( ListT1 )subject)))) {


        assertEquals(tom_get_head_concT1_ListT1((( ListT1 )subject)), tom_make_a());
        return;
      }}}}}

    fail();
  }

  public void test_listMatchLast() {
    Object subject = tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1())));

    {{if (tom_is_sort_ListT1(subject)) {if (tom_is_fun_sym_concT1((( ListT1 )subject))) { ListT1  tomMatch7__end__4=(( ListT1 )subject);do {{if (!(tom_is_empty_concT1_ListT1(tomMatch7__end__4))) {if (tom_is_empty_concT1_ListT1(tom_get_tail_concT1_ListT1(tomMatch7__end__4))) {


        assertEquals(tom_get_head_concT1_ListT1(tomMatch7__end__4), tom_make_f(tom_make_a(),tom_make_g(tom_make_b())));
        return;
      }}if (tom_is_empty_concT1_ListT1(tomMatch7__end__4)) {tomMatch7__end__4=(( ListT1 )subject);} else {tomMatch7__end__4=tom_get_tail_concT1_ListT1(tomMatch7__end__4);}}} while(!(tom_equal_term_ListT1(tomMatch7__end__4, (( ListT1 )subject))));}}}}

    fail();
  }

  public void test_listMatchCount() {
    T2 subject = tom_make_h(tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1()))));

    int i = 0;
    {{if (tom_is_sort_T2(subject)) {if (tom_is_fun_sym_h((( T2 )subject))) { ListT1  tomMatch8_1=tom_get_slot_h_ts((( T2 )subject));if (tom_is_fun_sym_concT1(tomMatch8_1)) { ListT1  tomMatch8__end__6=tomMatch8_1;do {{if (!(tom_is_empty_concT1_ListT1(tomMatch8__end__6))) {

        i++;
      }if (tom_is_empty_concT1_ListT1(tomMatch8__end__6)) {tomMatch8__end__6=tomMatch8_1;} else {tomMatch8__end__6=tom_get_tail_concT1_ListT1(tomMatch8__end__6);}}} while(!(tom_equal_term_ListT1(tomMatch8__end__6, tomMatch8_1)));}}}}}

    assertEquals(i,3);
  }

  public void test_listMatchNonLinear() {
    T2 subject = tom_make_h(tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1()))))));

    {{if (tom_is_sort_T2(subject)) {if (tom_is_fun_sym_h((( T2 )subject))) { ListT1  tomMatch9_1=tom_get_slot_h_ts((( T2 )subject));if (tom_is_fun_sym_concT1(tomMatch9_1)) { ListT1  tomMatch9__end__6=tomMatch9_1;do {{if (!(tom_is_empty_concT1_ListT1(tomMatch9__end__6))) { T1  tom_x=tom_get_head_concT1_ListT1(tomMatch9__end__6); ListT1  tomMatch9_7=tom_get_tail_concT1_ListT1(tomMatch9__end__6); ListT1  tomMatch9__end__10=tomMatch9_7;do {{if (!(tom_is_empty_concT1_ListT1(tomMatch9__end__10))) {if (tom_equal_term_T1(tom_x, tom_get_head_concT1_ListT1(tomMatch9__end__10))) {

        assertEquals(tom_x,tom_make_a());
        assertEquals(tom_get_slice_concT1(tomMatch9_1,tomMatch9__end__6,tom_empty_list_concT1()),tom_empty_list_concT1());
        assertEquals(tom_get_slice_concT1(tomMatch9_7,tomMatch9__end__10,tom_empty_list_concT1()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_empty_list_concT1())));
        assertEquals(tom_get_tail_concT1_ListT1(tomMatch9__end__10),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1()));
        return;
      }}if (tom_is_empty_concT1_ListT1(tomMatch9__end__10)) {tomMatch9__end__10=tomMatch9_7;} else {tomMatch9__end__10=tom_get_tail_concT1_ListT1(tomMatch9__end__10);}}} while(!(tom_equal_term_ListT1(tomMatch9__end__10, tomMatch9_7)));}if (tom_is_empty_concT1_ListT1(tomMatch9__end__6)) {tomMatch9__end__6=tomMatch9_1;} else {tomMatch9__end__6=tom_get_tail_concT1_ListT1(tomMatch9__end__6);}}} while(!(tom_equal_term_ListT1(tomMatch9__end__6, tomMatch9_1)));}}}}{if (tom_is_sort_T2(subject)) {

        fail();
      }}}

  }

  public void test_listMatchAny() {
    T2 subject = tom_make_h(tom_cons_list_concT1(tom_make_a(),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_b()),tom_cons_list_concT1(tom_make_f(tom_make_a(),tom_make_g(tom_make_b())),tom_empty_list_concT1()))));

    {{if (tom_is_sort_T2(subject)) {boolean tomMatch10_10= false ;if (tom_is_fun_sym_h((( T2 )subject))) { ListT1  tomMatch10_1=tom_get_slot_h_ts((( T2 )subject));if (tom_is_fun_sym_concT1(tomMatch10_1)) { ListT1  tomMatch10__end__6=tomMatch10_1;do {{if (!(tom_is_empty_concT1_ListT1(tomMatch10__end__6))) {if (tom_is_fun_sym_a(tom_get_head_concT1_ListT1(tomMatch10__end__6))) {tomMatch10_10= true ;}}if (tom_is_empty_concT1_ListT1(tomMatch10__end__6)) {tomMatch10__end__6=tomMatch10_1;} else {tomMatch10__end__6=tom_get_tail_concT1_ListT1(tomMatch10__end__6);}}} while(!(tom_equal_term_ListT1(tomMatch10__end__6, tomMatch10_1)));}}if (!(tomMatch10_10)) {

        fail();
        return;
      }}}}

  }

  /**
    public void testCongruence() {
    T1 subject = `f(f(a(),b()),g(b()));
    try {
    T1 res1 = (T1) `_f(Rule(), Rule2()).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
    T1 res2 = (T1) `_f(Rule(), Rule2()).visit(subject,tom.library.oomapping.Introspector.getInstance());
    assertEquals(res1, `f(a(),b()));
    assertEquals(res1, res2);
    } catch(VisitFailure e) {
    fail();
    }
    }

    public void test_listMatchCongruence() {
    List<T1> subject = `concT1(f(a(),b()),a(),f(a(),b()));
    try {
    List<T1> res1 = (List<T1>) `_concT1(Try(Rule())).visitLight(subject,tom.library.oomapping.Introspector.getInstance());
    List<T1> res2 = (List<T1>) `_concT1(Try(Rule())).visit(subject,tom.library.oomapping.Introspector.getInstance());
  //List<T1> res2 = (List<T1>) `_concT1(Try(Sequence(Print(),Rule()))).visit(subject,tom.library.oomapping.Introspector.getInstance());
  assertEquals(res1, `concT1(a(),a(),a()));
  //assertEquals(res1, res2);
  } catch(VisitFailure e) {
  fail();
  }
  }

  public void test_congWithListMatch() {
  T2 subject = `h(concT1(a(), f(a(), b()), f(a(), g(b()))));

// todo: how to do this?
//`_h(_concT1(map(Print()))).visitLight(subject);
}
   */
}
