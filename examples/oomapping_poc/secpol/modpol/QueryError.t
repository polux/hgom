package modpol;

public class QueryError implements Query{
  static public QueryError instance;

  static { instance = new QueryError(); }
}
