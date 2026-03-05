import FlowTest.Core.ToListTests
import FlowTest.Core.ForEachTests
import FlowTest.Core.MapTests
import FlowTest.Core.FilterTests
import FlowTest.Core.BuildersTests
import FlowTest.Core.SharedFlowTests
import FlowTest.Core.StateFlowTests

namespace CoreTests

def allTests : List (String × IO Unit) :=
  ToListTests.allTests
  ++ ForEachTests.allTests
  ++ MapTests.allTests
  ++ FilterTests.allTests
  ++ BuildersTests.allTests
  ++ SharedFlowTests.allTests
  ++ StateFlowTests.allTests

end CoreTests
