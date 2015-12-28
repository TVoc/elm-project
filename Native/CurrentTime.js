Elm.Native.CurrentTime = {};

Elm.Native.CurrentTime.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};


  localRuntime.Native.CurrentTime = localRuntime.Native.CurrentTime || {};

  if (localRuntime.Native.CurrentTime.values) {
    return localRuntime.Native.CurrentTime.values;
  }

  //var Result = Elm.Result.make(localRuntime);

  return localRuntime.Native.CurrentTime.values = {
    loadTime: (new window.Date).getTime()
  };

};