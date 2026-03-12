import FlowTest.Core.ToListTests
import FlowTest.Core.SubscribeTests
import FlowTest.Core.MapTests
import FlowTest.Core.FilterTests
import FlowTest.Core.BuildersTests
import FlowTest.Core.SharedFlowTests
import FlowTest.Core.StateFlowTests
import FlowTest.Core.ProgramFlowTests

namespace CoreTests

def allTests : List (String × IO Unit) :=
  ToListTests.allTests
  ++ SubscribeTests.allTests
  ++ MapTests.allTests
  ++ FilterTests.allTests
  ++ BuildersTests.allTests
  ++ SharedFlowTests.allTests
  ++ StateFlowTests.allTests
  ++ ProgramFlowTests.allTests

end CoreTests
