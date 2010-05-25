package poltemps;
import modpol.*;
import polrbac.*;

class QueryTemps implements Query{

public int hour;
public int type; /*@ invariant == 0 \/ == 1 @*/
/*@           |-> start \/ |-> end @*/
public Query subquery;

public QueryTemps(Query a,int h){
subquery = a; hour = h;
if(a instanceof QueryCreateSession) type = 0;
else{
if(a instanceof QueryDeleteSession) type = 1;
else System.err.println("Error in QueryTemps::QueryTemps(Query,int)");
}
}
}
