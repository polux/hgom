package modpol;
import tom.library.sl.*;

abstract public class Policy extends AbstractStrategyBasic {


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
private static  tom.library.sl.Strategy  tom_make_Identity() { 
return ( new tom.library.sl.Identity() );
}
private static boolean tom_equal_term_Query(Object c1, Object c2) {
return  c1.equals(c2) ;
}
private static boolean tom_is_sort_Query(Object c) {
return  c instanceof Query ;
}


public Policy(){
super(
tom_make_Identity());
}

// to be overridden
//  %strategy Default() extends Identity(){
//    visit Query {
//      q@_ -> { return null; }
//    }
//  }  

// to be overridden
public abstract Query compute(Query q);
public abstract tom.library.sl.Introspector getPolicyIntrospector();

public <T> T visitLight(T req,Introspector introspector) throws VisitFailure{
if(req instanceof Query){
Query q = (Query) req;
return (T)compute(q); // this is bullshit
}
else return any.visitLight(req,introspector); //throw new VisitFailure("Policy::visitLight(T,Introspector): first argument not of type Query");
}



}
