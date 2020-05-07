# GoAuto

# What is this

This is a way of having UiAutomator tests running in a dynamic way.

Tests are written in a simple interpreted language in your computer. The program is sent to the device through 'adb shell am instrument' and it will run
within a UiAutomator test in the device.


# Example

You first have to run the InstrumentedTest.mainTest in Android Studio so the device has an instrumented apk installed (or run the corresponding gradle tasks :app:assembleDebug, :app:assembleDebugAndroidTest)


The program.auto file contains:

```
tap_with_id("floating_action_button");

var text = "";

var i = 4;
while(i > 0) {
	text = text + " " + str(i);
	i = i - 1;
}

text = text + " ... " + "go";

write_text(text);

tap_with_text("SAVE");

press_back();
```

Run in bash:

```
# The program is passed as text to the device so currently you will have to escape some characters first.

PROGRAM=$(cat program.auto)
PROGRAM=$(printf "%q" $PROGRAM)
adb shell am instrument -w -r -e debug false -e class 'com.gplio.goauto.InstrumentedTest#mainTest' -e program $PROGRAM com.gplio.goauto.test/androidx.test.runner.AndroidJUnitRunner
```

This will run the following scenario:

- Tap a view with id "floating_action_button".
- Write "4 3 2 1 ... go" in a focused text view.
- Tap a button that has the text "SAVE".
- Will press the back button.


# Why

Recompiling is slow and thus annoying.

Starting up a test with 'adb shell am instrument' is also slow. Currently it's required that it's done everytime but it will be changed in the future.
There's other similar projects that block a single UiAutomator test to continously receive new updates, this is probably where this will end up.


# Is it finished

Currently super underbaked. The interpreted language is too simple and the bindings are super limited right now (InstrumentedTest.getFunctions)


# Language

The interpreter that is used to run the scripts started as a Lox kotlin implementation from https://craftinginterpreters.com/ but it's currently changing to turn it into a friendlier language for scripts.


# Similar projects

TODO
