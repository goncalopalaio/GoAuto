package com.gplio.goauto

import Interpreter
import LangCallable
import android.content.Context
import android.util.Log
import androidx.test.ext.junit.runners.AndroidJUnit4
import androidx.test.platform.app.InstrumentationRegistry
import androidx.test.uiautomator.*
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import runProgram
import java.util.regex.Pattern

data class InstrumentedTestArguments(val program: String)

@RunWith(AndroidJUnit4::class)
class InstrumentedTest {

    private lateinit var context: Context
    private lateinit var device: UiDevice
    private var arguments = InstrumentedTestArguments("")

    private fun getArguments(): InstrumentedTestArguments {
        val bundle = InstrumentationRegistry.getArguments()
        var res = bundle.get("program")
        res = if (res != null) {
            res as String
        } else {
            ""
        }

        return InstrumentedTestArguments(res)
    }

    private fun writeTextInFocused(text: String) {
        val objs = device.findObjects(By.focused(true))
        objs?.let {
            objs.forEach { tryWriteInput(text, it) }
        }
    }

    private fun tryWriteInput(text: String, input: UiObject2) {
        for (i in 0..2) {
            try {
                log("tryWriteInput text: $text")
                input.text = text
                return
            } catch (e: UiObjectNotFoundException) {
                log("tryWriteInput UiObject not found")
            }
        }
    }

    private fun tapViewWithText(text: String) {
        device.findObject(UiSelector().text(text))?.click()
    }

    private fun tapViewWithId(id: String) {
        val ets = device.findObjects(By.res(Pattern.compile("(.)*($id)$")))
        ets.forEach {
            it.click()
        }
    }

    /**
     * Function bindings
     */
    private fun getFunctions(): ArrayList<Pair<String, LangCallable>> {
        val functions = arrayListOf<Pair<String, LangCallable>>()
        functions.add(Pair<String, LangCallable>("write_text", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                val text = arguments[0] as String
                log("Running write_text $text")
                writeTextInFocused(text)
                return text
            }

            override fun arity(): Int = 1
        }))

        functions.add(Pair<String, LangCallable>("tap_with_text", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                val text = arguments[0] as String
                log("Running tap_with_text $text")
                tapViewWithText(text)
                return text
            }

            override fun arity(): Int = 1
        }))

        functions.add(Pair<String, LangCallable>("tap_with_id", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                val text = arguments[0] as String
                log("Running tap_with_id $text")
                tapViewWithId(text)
                return text
            }

            override fun arity(): Int = 1
        }))

        functions.add(Pair<String, LangCallable>("press_back", object : LangCallable {
            override fun call(interpreter: Interpreter, arguments: List<Any>): Any {
                log("Running press_back")
                device.pressBack()
                return 0
            }

            override fun arity(): Int = 0
        }))
        //
        return functions
    }

    @Before
    fun prepareTest() {
        val instrumentation = InstrumentationRegistry.getInstrumentation()
        device = UiDevice.getInstance(instrumentation)
        context = instrumentation.targetContext
        arguments = getArguments()
    }

    @Test
    fun mainTest() {
        val arguments = getArguments()
        log("arguments: $arguments")
        runProgram(arguments.program, getFunctions())
    }

    private fun log(text: String) {
        Log.d("GoAuto", "InstrumentedTest: $text")
    }
}
