
#include "clang/Sema/SemaInternal.h"
#include "clang/AST/StmtCXX.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"

using namespace clang;
using namespace clang::ast_matchers;

namespace {

StatementMatcher callExprMatcher = findAll(
    callExpr().bind("call_expr"));

class TransactionAtomicStmtVisitor :
    public RecursiveASTVisitor<TransactionAtomicStmtVisitor> {
  Sema& SemaRef;
  unsigned DiagID;
  class CallExprMatcherCallback : public MatchFinder::MatchCallback {
    Sema& SemaRef;
    unsigned DiagID;
    virtual void run(const MatchFinder::MatchResult &Result) {
      if (const CallExpr* callExpr =
          Result.Nodes.getNodeAs<CallExpr>("call_expr")) {
        const Decl* callee = callExpr->getCalleeDecl();
        if (callee != nullptr) {
          const FunctionDecl* FD = callee->getAsFunction();
          if (FD != nullptr) {
            if (FD->getName().compare("malloc") == 0 ||
                FD->getName().compare("calloc") == 0 ||
                FD->getName().compare("free") == 0) {
              return;
            }
          }
          if (!callee->hasAttr<TransactionSafeAttr>() &&
              !callee->hasAttr<TransactionPureAttr>()) {
            SemaRef.Diag(callExpr->getLocStart(),
                DiagID);
          }
        }
      }
    }
  public:
    CallExprMatcherCallback(Sema &SemaRef, unsigned DiagID) :
        SemaRef(SemaRef), DiagID(DiagID) {}
  };
public:
  TransactionAtomicStmtVisitor(Sema &SemaRef, unsigned DiagID) :
      SemaRef(SemaRef), DiagID(DiagID) {}
  void VisitCompoundStmt(CompoundStmt *CS) {
    llvm::errs() << "Hello from [TransactionAtomicStmtVisitor]\n";
    MatchFinder finder;
    CallExprMatcherCallback callback(SemaRef, DiagID);
    finder.addMatcher(callExprMatcher, &callback);
    finder.match(*CS, SemaRef.getASTContext());
  }
};
}

void Sema::DiagnoseUnsafeCalls(Stmt* Body, unsigned DiagID) {
  llvm::errs() << "Hello from [DiagnoseUnsafeCalls]\n";
  TransactionAtomicStmtVisitor(*this, DiagID)
    .VisitCompoundStmt(cast<CompoundStmt>(Body));
}
