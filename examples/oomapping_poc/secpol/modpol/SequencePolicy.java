package modpol;
import tom.library.sl.*; // imports the runtime strategy library

// pour l'exemple
import poltemps.*;

public class SequencePolicy extends Policy{


private static boolean tom_equal_term_Query(Object c1, Object c2) {
return  c1.equals(c2) ;
}
private static boolean tom_is_sort_Query(Object c) {
return  c instanceof Query ;
}

// includes description of strategy operators for tom 

private static boolean tom_equal_term_Strategy(Object t1, Object t2) {
return  (t1.equals(t2)) ;
}
private static boolean tom_is_sort_Strategy(Object t) {
return  (t instanceof tom.library.sl.Strategy) ;
}
private static boolean tom_equal_term_Position(Object t1, Object t2) {
return  (t1.equals(t2)) ;
}
private static boolean tom_is_sort_Position(Object t) {
return  (t instanceof tom.library.sl.Position) ;
}
private static boolean tom_equal_term_int(int t1, int t2) {
return  t1==t2 ;
}
private static boolean tom_is_sort_int(int t) {
return  true ;
}
private static boolean tom_equal_term_char(char t1, char t2) {
return  t1==t2 ;
}
private static boolean tom_is_sort_char(char t) {
return  true ;
}
private static boolean tom_equal_term_String(String t1, String t2) {
return  t1.equals(t2) ;
}
private static boolean tom_is_sort_String(String t) {
return  t instanceof String ;
}
private static boolean tom_is_fun_sym_Sequence( tom.library.sl.Strategy  t) {
return ( (t instanceof tom.library.sl.Sequence) );
}
private static  tom.library.sl.Strategy  tom_empty_list_Sequence() { 
return ( null );
}
private static  tom.library.sl.Strategy  tom_cons_list_Sequence( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { 
return ( (tail==null)?head:new tom.library.sl.Sequence(head,tail) );
}
private static  tom.library.sl.Strategy  tom_get_head_Sequence_Strategy( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Sequence.FIRST) );
}
private static  tom.library.sl.Strategy  tom_get_tail_Sequence_Strategy( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Sequence.THEN) );
}
private static boolean tom_is_empty_Sequence_Strategy( tom.library.sl.Strategy  t) {
return ( t == null );
}

  private static   tom.library.sl.Strategy  tom_append_list_Sequence( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {
    if(( l1 == null )) {
      return l2;
    } else if(( l2 == null )) {
      return l1;
    } else if(( (l1 instanceof tom.library.sl.Sequence) )) {
      if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ) == null )) {
        return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ):new tom.library.sl.Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ),l2) );
      } else {
        return ( (tom_append_list_Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ):new tom.library.sl.Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.FIRST) ),tom_append_list_Sequence(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Sequence.THEN) ),l2)) );
      }
    } else {
      return ( (l2==null)?l1:new tom.library.sl.Sequence(l1,l2) );
    }
  }
  private static   tom.library.sl.Strategy  tom_get_slice_Sequence( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {
    if( (begin.equals(end)) ) {
      return tail;
    } else if( (end.equals(tail))  && (( end == null ) ||  (end.equals(tom_empty_list_Sequence())) )) {
      /* code to avoid a call to make, and thus to avoid looping during list-matching */
      return begin;
    }
    return ( (( tom.library.sl.Strategy )tom_get_slice_Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.THEN) ):tom_empty_list_Sequence()),end,tail)==null)?((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.FIRST) ):begin):new tom.library.sl.Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_Sequence(((( (begin instanceof tom.library.sl.Sequence) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Sequence.THEN) ):tom_empty_list_Sequence()),end,tail)) );
  }
  

private Policy pol1;
private Policy pol2;

public SequencePolicy(Policy p1,Policy p2){ pol1 = p1; pol2 = p2; }

//  %strategy First() extends Identity(){
//    visit Query{
//      q@query() -> { pol1.query(`q); }
//    }
//  }


public Query compute(Query q){
//TODO: should be set in the classes of pol1 and pol2
pol1.setIntrospector(pol1.getPolicyIntrospector());
pol2.setIntrospector(pol2.getPolicyIntrospector());

try {
return 
tom_cons_list_Sequence(pol1,tom_cons_list_Sequence(pol2,tom_empty_list_Sequence())).visit(q, null);
/**    
Query q1 = pol1.visit(q,pol1.getPolicyIntrospector());
if (q1 instanceof Permit) {
return q1;
} else {
return pol2.visit(q,pol2.getPolicyIntrospector());
}
*/
} catch (tom.library.sl.VisitFailure e) {
throw new RuntimeException("unexpected visitfailure "+e);
}

}

public tom.library.sl.Introspector getPolicyIntrospector() {
return null;
}
//    // Query answer1 = pol1.query(q);
//    PolTemps p1 = (PolTemps) pol1;
//    Strategy s1 = p1.getStrat();
//    PolTemps p2 = (PolTemps) pol2;
//    Strategy s2 = p2.getStrat();
//    Query answer2 = pol2.query(q);
//    Strategy s2 = `Identity.visitLight(answer2,new LocalIntrospector());
//    return `Sequence(s1,s2).visit(q,new LocalIntrospector());
//    return null;
//  }
//    Query answer1 = pol1.query(q);
//    Query answer2 = pol2.query(q);
//
//    // /* debug */ System.out.println("tmpstate: "+answer1+" / "+answer2);
//
//    %match(Query answer1, Query answer2){
//      permit,_ -> { return `permit(); }
//      _,permit -> { return `permit(); }
//      deny,deny -> { return `deny(); }
//      deny,notApplicable -> { return `deny(); }
//      notApplicable,deny -> { return `deny(); }
//      notApplicable,notApplicable -> { return `deny(); } 
//    }
//    System.err.println("Error in PermitOverrides::query(Query)");
//    return null;
//  }
}
