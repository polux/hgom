package jboss_el;

import org.jboss.el.parser.*;

import junit.framework.TestCase;
import junit.framework.TestSuite;
import junit.framework.Assert;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

public class TestEL extends TestCase {

  %include { el/EL.tom }
  %include { sl.tom }
  %include { string.tom }
  %include { java/util/types/Collection.tom }
  %include { java/util/HashSet.tom }

  public static void main(String[] args) {
    junit.textui.TestRunner.run(new TestSuite(TestEL.class));
  }

  public void testIdentifier() {
    Node node = ELParser.parse("#{contactSearchContext}");

    final List<Node> list = collectTerms(node, AstIdentifier.class);

    Assert.assertTrue(list.size() == 1 && list.get(0) instanceof AstIdentifier);
  }

  public void testIdentifierAndProperties() {
    Node node = ELParser.parse("#{contactSearchContext.positionGroup.syncUpdateDt}");
    final List<Node> list = collectTerms(node, AstIdentifier.class, AstPropertySuffix.class);


    Assert.assertTrue(list.size() == 3 && list.get(0) instanceof AstIdentifier &&
        list.get(1) instanceof AstPropertySuffix && list.get(2) instanceof AstPropertySuffix);
  }

  public void testEquals() {
    Node node = ELParser.parse("#{communicationContext.communication.id == null}");
    final List<Node> list = collectTerms(node);
    Assert.assertTrue(list.size() == 4 && list.get(0) instanceof AstEqual);
  }

  public void testArithmetic() {
    Node node = ELParser.parse("#{a + 2 / 3}");
    final List<Node> list = collectTerms(node);
    Assert.assertTrue(list.size() == 5 && list.get(0) instanceof AstPlus);
  }

  public void testConditional() {
    Node node = ELParser.parse("#{a != 3  ? 2 : 3}");
    final List<Node> list = collectTerms(node);
    Assert.assertTrue(list.size() == 6 && list.get(0) instanceof AstChoice);
  }

  public void testFunctionCall() {
    Node node = ELParser.parse("#{communicationContext.abc.search(searchEntityId,searchEntityCode)}");
    final List<Node> list = collectTerms(node);
    Assert.assertTrue(list.size() == 5 && list.get(0) instanceof AstValue);
  }

  public void testFunctionCallConst() {
    Node node = ELParser.parse("#{common.selectItems('Jurisdiction')}");
    final List<Node> list = collectTerms(node);
    Assert.assertTrue(list.size() == 3 && list.get(0) instanceof AstValue);
  }

  public void testNotEqual() {
    Node node = ELParser.parse("#{'query'!=contactSearchContext.contactFindType}");
    final List<Node> list = collectTerms(node);

    Assert.assertTrue(list.size() == 4 && list.get(0) instanceof AstNotEqual);
  }

  public void testNotEmpty() {
    Node node = ELParser.parse("#{not empty search.results}");
    final List<Node> list = collectTerms(node);

    Assert.assertTrue(list.size() == 4 && list.get(0) instanceof AstNot);
  }

  public void testMixedLiteral() {
    Node node = ELParser.parse("#{contactSearchList.getRowCount()} Clients Total Assets = $ #{contactSearchContext.getTotalAsset()}");
    final List<Node> list = collectTerms(node);

    Assert.assertTrue(list.size() == 5 && list.get(2) instanceof AstLiteralExpression);
  }

  public void testGreaterThan() {
    Node node = ELParser.parse("#{contacts.rowCount>0}");
    final List<Node> list = collectTerms(node);

    Assert.assertTrue(list.size() == 4 && list.get(0) instanceof AstGreaterThan);
  }

  public void testArrayAccess() {
    Node node = ELParser.parse("/images/icons/security/check_#{fsiContext.codeMap[fsiContext.getKey(fs.id, accessType)]}.PNG");
    final List<Node> list = collectTerms(node);

    Assert.assertTrue(list.size() == 10 && list.get(1) instanceof AstValue);
  }

  public void testReplaceLiteral() {
    Node node = ELParser.parse("#{contactSearchList.getRowCount()} Clients Total Assets = $ #{contactSearchContext.getTotalAsset()}");
    try {
      node = `BottomUp(ReplaceLiteral()).visitLight(node, new LocalIntrospector());
      %match(node) {
        !CompositeExpression(concExpr(_, 
                                    FunctionCall(concExpr(Identifier("bundle")),"get",concExpr(Literal(" Clients Total Assets = $ ")))
                                    , _)) -> { fail(); }
      }
    } catch(tom.library.sl.VisitFailure e ) { fail(); }
  }

  public void testCollectSuspectCall() {
    Node node = ELParser.parse("#{contactSearchList.getRowCount()} #{contactSearchList.removeContact(Pierre)} #{contactSearchContext.renameContact(Pierre,Paul)}");
    try {
      HashSet bag = new HashSet();
      `TopDown(CollectSuspectMethodCall(bag)).visitLight(node, new LocalIntrospector());
      Assert.assertTrue(bag.size() == 2);
    } catch(tom.library.sl.VisitFailure e ) { fail(); }
  }

  public void testCollectLeft() {
    Node node = ELParser.parse("#{1 and 'abc'}");

    try {
      Node res = (Node) `Repeat(OnceBottomUp(CollectLeft())).visitLight(node, new LocalIntrospector());
      System.out.println("res = " + res);
    } catch(tom.library.sl.VisitFailure e) {
      System.out.println("failure");
    }

  }

  %strategy ReplaceLiteral() extends Identity() {
    visit Expr {
      Literal(s) -> FunctionCall(concExpr(Identifier("bundle")),"get",concExpr(Literal(s)))
    }
  }

  %strategy CollectSuspectMethodCall(bag:HashSet) extends Identity() {
    visit Expr {
      f@FunctionCall[name=!concString('get',_*)] -> { bag.add(`f); }
    }
  }


  %strategy FindTerms(terms:Collection, cls:Collection) extends Identity() {
    visit Expr {
      e@(ArrayAccess|FunctionCall|LessThanEqual|LessThan|GreaterThanEqual|GreaterThan|If|Empty|Not_|NotEqual|Equal|PropertySuffix|Div|Mult|Minus|Plus|Str|Literal|Identifier|Integer)[] -> {
        if(cls.isEmpty() || cls.contains(`e.getClass())) {
          terms.add(`e);
        }
      }
    }
  }

  %strategy Debug() extends Identity() {
    visit Expr {
      x -> {
        System.out.println(getPosition());
        System.out.println(`x+"\n");
      }
    }
    visit ExprList {
      x -> {
        System.out.println(getPosition());
        System.out.println(`x+"\n");
      }
    }
  }

  public static List<Node> collectTerms(Node source, Class... cls) {
    try {
      List<Node> exprs = new ArrayList<Node>();
      //`TopDown(Sequence(Debug(),FindTerms(exprs, Arrays.asList(cls)))).visit(source, new LocalIntrospector());
      `TopDown(FindTerms(exprs, Arrays.asList(cls))).visitLight(source, new LocalIntrospector());
      return exprs;
    }catch(tom.library.sl.VisitFailure e){
      throw new RuntimeException(e);
    }
  }

  %strategy CollectLeft() extends Fail() {
    visit Expr {
      And(l, r) -> {
        return `l;
      }
    }
  }

}
