package polrbac;
import modpol.*;
import tom.library.sl.*;

class PolicyRBAC extends Policy{


private static boolean tom_equal_term_Query(Object c1, Object c2) {
return  c1.equals(c2) ;
}
private static boolean tom_is_sort_Query(Object c) {
return  c instanceof Query ;
}
private static boolean tom_is_fun_sym_permit( Query  c) {
return  c instanceof Permit ;
}
private static  Query  tom_make_permit() { 
return  Permit.instance ;
}
private static boolean tom_is_fun_sym_notApplicable( Query  c) {
return  c instanceof NotApplicable ;
}
private static  Query  tom_make_notApplicable() { 
return  NotApplicable.instance ;
}
private static boolean tom_is_fun_sym_queryCreateSession( Query  c) {
return  c instanceof QueryCreateSession ;
}
private static  Query  tom_make_queryCreateSession() { 
return  new QueryCreateSession() ;
}
private static boolean tom_is_fun_sym_queryDeleteSession( Query  c) {
return  c instanceof QueryDeleteSession ;
}
private static  Query  tom_make_queryDeleteSession() { 
return  new QueryDeleteSession() ;
}
private static boolean tom_equal_term_Strategy(Object t1, Object t2) {
return  (t1.equals(t2)) ;
}
private static boolean tom_is_sort_Strategy(Object t) {
return  (t instanceof tom.library.sl.Strategy) ;
}
private static boolean tom_is_fun_sym_Identity( tom.library.sl.Strategy  t) {
return ( (t instanceof tom.library.sl.Identity) );
}
private static  tom.library.sl.Strategy  tom_make_Identity() { 
return ( new tom.library.sl.Identity() );
}
private static boolean tom_is_fun_sym_Fail( tom.library.sl.Strategy  t) {
return ( (t instanceof tom.library.sl.Fail) );
}
private static  tom.library.sl.Strategy  tom_make_Fail() { 
return ( new tom.library.sl.Fail() );
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
  private static boolean tom_is_fun_sym_IfThenElse( tom.library.sl.Strategy  t) {
return ( (t instanceof tom.library.sl.IfThenElse) );
}
private static  tom.library.sl.Strategy  tom_make_IfThenElse( tom.library.sl.Strategy  condition,  tom.library.sl.Strategy  trueCase,  tom.library.sl.Strategy  falseCase) { 
return ( new tom.library.sl.IfThenElse(condition,trueCase,falseCase) );
}
private static  tom.library.sl.Strategy  tom_get_slot_IfThenElse_s1( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.IfThenElse.CONDITION) );
}
private static  tom.library.sl.Strategy  tom_get_slot_IfThenElse_s2( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.IfThenElse.TRUE_CASE) );
}
private static  tom.library.sl.Strategy  tom_get_slot_IfThenElse_s3( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.IfThenElse.FALSE_CASE) );
}
private static boolean tom_is_fun_sym_Choice( tom.library.sl.Strategy  t) {
return ( (t instanceof tom.library.sl.Choice) );
}
private static  tom.library.sl.Strategy  tom_empty_list_Choice() { 
return ( null );
}
private static  tom.library.sl.Strategy  tom_cons_list_Choice( tom.library.sl.Strategy  head,  tom.library.sl.Strategy  tail) { 
return ( (tail==null)?head:new tom.library.sl.Choice(head,tail) );
}
private static  tom.library.sl.Strategy  tom_get_head_Choice_Strategy( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Choice.FIRST) );
}
private static  tom.library.sl.Strategy  tom_get_tail_Choice_Strategy( tom.library.sl.Strategy  t) {
return ( (tom.library.sl.Strategy)t.getChildAt(tom.library.sl.Choice.THEN) );
}
private static boolean tom_is_empty_Choice_Strategy( tom.library.sl.Strategy  t) {
return ( t ==null );
}

  private static   tom.library.sl.Strategy  tom_append_list_Choice( tom.library.sl.Strategy  l1,  tom.library.sl.Strategy  l2) {
    if(( l1 ==null )) {
      return l2;
    } else if(( l2 ==null )) {
      return l1;
    } else if(( (l1 instanceof tom.library.sl.Choice) )) {
      if(( ( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ) ==null )) {
        return ( (l2==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ):new tom.library.sl.Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ),l2) );
      } else {
        return ( (tom_append_list_Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ),l2)==null)?( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ):new tom.library.sl.Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.FIRST) ),tom_append_list_Choice(( (tom.library.sl.Strategy)l1.getChildAt(tom.library.sl.Choice.THEN) ),l2)) );
      }
    } else {
      return ( (l2==null)?l1:new tom.library.sl.Choice(l1,l2) );
    }
  }
  private static   tom.library.sl.Strategy  tom_get_slice_Choice( tom.library.sl.Strategy  begin,  tom.library.sl.Strategy  end, tom.library.sl.Strategy  tail) {
    if( (begin.equals(end)) ) {
      return tail;
    } else if( (end.equals(tail))  && (( end ==null ) ||  (end.equals(tom_empty_list_Choice())) )) {
      /* code to avoid a call to make, and thus to avoid looping during list-matching */
      return begin;
    }
    return ( (( tom.library.sl.Strategy )tom_get_slice_Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.THEN) ):tom_empty_list_Choice()),end,tail)==null)?((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.FIRST) ):begin):new tom.library.sl.Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.FIRST) ):begin),( tom.library.sl.Strategy )tom_get_slice_Choice(((( (begin instanceof tom.library.sl.Choice) ))?( (tom.library.sl.Strategy)begin.getChildAt(tom.library.sl.Choice.THEN) ):tom_empty_list_Choice()),end,tail)) );
  }
  

//  %include { sl.tom }
//
//  %strategy Default() extends Identity(){
//    visit Query{
//	   queryCreateSession() -> { return `permit(); }
//	   queryDeleteSession() -> { return `permit(); }
//	   _ -> { return `notApplicable(); }
//    }
//    System.err.println("Error in PolicyRBAC::query(), matching failed!");
//    return null;
//  } 


public static class LocalIntrospector implements tom.library.sl.Introspector {@SuppressWarnings("unchecked")
public int getChildCount(Object o)
 {
if (o==null) {
return 0;
}
if (tom_is_sort_Query(o)) {
if (tom_is_fun_sym_notApplicable((( Query )o))) {
return 0;
}
if (tom_is_fun_sym_queryDeleteSession((( Query )o))) {
return 0;
}
if (tom_is_fun_sym_permit((( Query )o))) {
return 0;
}
if (tom_is_fun_sym_queryCreateSession((( Query )o))) {
return 0;
}

}
if (tom_is_sort_Strategy(o)) {
if (tom_is_fun_sym_Sequence((( tom.library.sl.Strategy )o))) {
if (tom_is_empty_Sequence_Strategy((( tom.library.sl.Strategy )o))) {
return 0;
} else {
return 2;
}
}
if (tom_is_fun_sym_MyStrat((( tom.library.sl.Strategy )o))) {
return 0;
}
if (tom_is_fun_sym_Fail((( tom.library.sl.Strategy )o))) {
return 0;
}
if (tom_is_fun_sym_Identity((( tom.library.sl.Strategy )o))) {
return 0;
}
if (tom_is_fun_sym_IfThenElse((( tom.library.sl.Strategy )o))) {
return 3;
}
if (tom_is_fun_sym_Choice((( tom.library.sl.Strategy )o))) {
if (tom_is_empty_Choice_Strategy((( tom.library.sl.Strategy )o))) {
return 0;
} else {
return 2;
}
}

}
return 0;

 }
@SuppressWarnings("unchecked")
public Object[] getChildren(Object o)
 {
if (tom_is_sort_Query(o)) {
if (tom_is_fun_sym_notApplicable((( Query )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_queryDeleteSession((( Query )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_permit((( Query )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_queryCreateSession((( Query )o))) {
return  new Object[]{};
}

}
if (tom_is_sort_Strategy(o)) {
if (tom_is_fun_sym_Sequence((( tom.library.sl.Strategy )o))) {
if (tom_is_empty_Sequence_Strategy((( tom.library.sl.Strategy )o))) {
return new Object[]{};
} else {
return new Object[]{tom_get_head_Sequence_Strategy((( tom.library.sl.Strategy )o)),tom_get_tail_Sequence_Strategy((( tom.library.sl.Strategy )o))};
}
}
if (tom_is_fun_sym_MyStrat((( tom.library.sl.Strategy )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_Fail((( tom.library.sl.Strategy )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_Identity((( tom.library.sl.Strategy )o))) {
return  new Object[]{};
}
if (tom_is_fun_sym_IfThenElse((( tom.library.sl.Strategy )o))) {
return  new Object[]{tom_get_slot_IfThenElse_s1((( tom.library.sl.Strategy )o)),tom_get_slot_IfThenElse_s2((( tom.library.sl.Strategy )o)),tom_get_slot_IfThenElse_s3((( tom.library.sl.Strategy )o))};
}
if (tom_is_fun_sym_Choice((( tom.library.sl.Strategy )o))) {
if (tom_is_empty_Choice_Strategy((( tom.library.sl.Strategy )o))) {
return new Object[]{};
} else {
return new Object[]{tom_get_head_Choice_Strategy((( tom.library.sl.Strategy )o)),tom_get_tail_Choice_Strategy((( tom.library.sl.Strategy )o))};
}
}

}
return null;

 }
@SuppressWarnings("unchecked")
public Object setChildren(Object o, Object[] children)
 {
if (tom_is_sort_Query(o)) {
if (tom_is_fun_sym_notApplicable((( Query )o))) {
return tom_make_notApplicable();
}
if (tom_is_fun_sym_queryDeleteSession((( Query )o))) {
return tom_make_queryDeleteSession();
}
if (tom_is_fun_sym_permit((( Query )o))) {
return tom_make_permit();
}
if (tom_is_fun_sym_queryCreateSession((( Query )o))) {
return tom_make_queryCreateSession();
}

}
if (tom_is_sort_Strategy(o)) {
if (tom_is_fun_sym_Sequence((( tom.library.sl.Strategy )o))) {
if (children.length==0) {
return tom_empty_list_Sequence();
} else {
return tom_cons_list_Sequence((( tom.library.sl.Strategy )children[0]),(( tom.library.sl.Strategy )children[1]));
}
}
if (tom_is_fun_sym_MyStrat((( tom.library.sl.Strategy )o))) {
return tom_make_MyStrat();
}
if (tom_is_fun_sym_Fail((( tom.library.sl.Strategy )o))) {
return tom_make_Fail();
}
if (tom_is_fun_sym_Identity((( tom.library.sl.Strategy )o))) {
return tom_make_Identity();
}
if (tom_is_fun_sym_IfThenElse((( tom.library.sl.Strategy )o))) {
return tom_make_IfThenElse((( tom.library.sl.Strategy )children[0]),(( tom.library.sl.Strategy )children[1]),(( tom.library.sl.Strategy )children[2]));
}
if (tom_is_fun_sym_Choice((( tom.library.sl.Strategy )o))) {
if (children.length==0) {
return tom_empty_list_Choice();
} else {
return tom_cons_list_Choice((( tom.library.sl.Strategy )children[0]),(( tom.library.sl.Strategy )children[1]));
}
}

}
return o;

 }
@SuppressWarnings("unchecked")
public Object getChildAt(Object o, int i)
 {
return getChildren(o)[i];
 }
@SuppressWarnings("unchecked")
public Object setChildAt(Object o, int i, Object child)
 {

            Object[] newChildren = getChildren(o);
            newChildren[i] = child;
            return newChildren;
           }
}public static class MyStrat extends tom.library.sl.AbstractStrategyBasic {
public MyStrat() {
super(tom_make_Identity());
}
public tom.library.sl.Visitable[] getChildren() {
tom.library.sl.Visitable[] stratChilds = new tom.library.sl.Visitable[getChildCount()];
stratChilds[0] = super.getChildAt(0);
return stratChilds;}
public tom.library.sl.Visitable setChildren(tom.library.sl.Visitable[] children) {
super.setChildAt(0, children[0]);
return this;
}
public int getChildCount() {
return 1;
}
public tom.library.sl.Visitable getChildAt(int index) {
switch (index) {
case 0: return super.getChildAt(0);
default: throw new IndexOutOfBoundsException();
}
}
public tom.library.sl.Visitable setChildAt(int index, tom.library.sl.Visitable child) {
switch (index) {
case 0: return super.setChildAt(0, child);
default: throw new IndexOutOfBoundsException();
}
}
@SuppressWarnings("unchecked")
public  Query  visit_Query( Query  tom__arg, tom.library.sl.Introspector introspector)
 throws tom.library.sl.VisitFailure {
{
{
if (tom_is_sort_Query(tom__arg)) {
System.out.println(
(( Query )tom__arg)); 

}

}

}
return _visit_Query(tom__arg,introspector);

 }
@SuppressWarnings("unchecked")
public  Query  _visit_Query( Query  arg, tom.library.sl.Introspector introspector)
 throws tom.library.sl.VisitFailure {
if (!((environment ==  null ))) {
return (( Query )any.visit(environment,introspector));
} else {
return any.visitLight(arg,introspector);
}
 }
@SuppressWarnings("unchecked")
public <T> T visitLight(T v, tom.library.sl.Introspector introspector)
 throws tom.library.sl.VisitFailure {
if (tom_is_sort_Query(v)) {
return ((T)visit_Query((( Query )v),introspector));
}
if (!((environment ==  null ))) {
return ((T)any.visit(environment,introspector));
} else {
return any.visitLight(v,introspector);
}

 }
}
private static  tom.library.sl.Strategy  tom_make_MyStrat() { 
return new MyStrat();
}
private static boolean tom_is_fun_sym_MyStrat( tom.library.sl.Strategy  t) {
return (t instanceof MyStrat);
}


public tom.library.sl.Introspector getPolicyIntrospector() {
return new LocalIntrospector();
}

public Query compute(Query q){

{
{
if (tom_is_sort_Query(q)) {
if (tom_is_fun_sym_queryCreateSession((( Query )q))) {
return 
tom_make_permit(); 

}
}

}
{
if (tom_is_sort_Query(q)) {
if (tom_is_fun_sym_queryDeleteSession((( Query )q))) {
return 
tom_make_permit(); 

}
}

}
{
if (tom_is_sort_Query(q)) {
return 
tom_make_notApplicable(); 

}

}


}

return null; // because the compiler sucks
} 


}
