// Copyright 2019 Philipp Klaus Krause, Albert-Ludwigs-Universität Freiburg
// Source code under CC0 1.0

#ifdef __APPLE__
#define USE_OPENGL_1_5_INSTEAD_OF_OPENGL_ES_1_1
#endif

#ifdef USE_OPENGL_1_5_INSTEAD_OF_OPENGL_ES_1_1
#define glOrthof glOrtho // OpenGL 1.5 hat kein glOrthof, aber ein glOrtho (dessen Argumente vom Typ double sind).
#else
#define GLFW_INCLUDE_ES1
#endif

#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <locale.h>

// In case of a GLFW error, just print an error message to stderr.
static void error_callback(int error, const char* description) {
    fprintf(stderr, "Es trat ein GLFW-Fehler auf: %s\n", description);
}

// Close the window when Esc is pressed
static void
key_click(GLFWwindow* window, int key, int scancode, int action, int mods) {
    if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS)
        glfwSetWindowShouldClose(window, GL_TRUE);
    if (action == GLFW_PRESS && key == GLFW_KEY_MINUS)
	printf("Minustaste gedrückt.\n");
    if(action ==GLFW_PRESS && key == GLFW_KEY_ENTER) {
        glRotatef(5.0f, 0.0f, 0.0f, 1.0f);
        glScalef(1.2, 1.0f, 1.0);
        }
}

void char_click(GLFWwindow *window, unsigned int codepoint)
{
    char s[MB_LEN_MAX + 1];
    wctomb(s, codepoint);
    printf("Texteingabe: %s\n", s);
    if (codepoint == L'-')
        printf("Minus eingegeben.\n");
}

void mouse_click(GLFWwindow* window, int button, int action, int mods)
{
    if (action == GLFW_PRESS && (button == GLFW_MOUSE_BUTTON_LEFT || GLFW_MOUSE_BUTTON_RIGHT))
    {
        double x, y;
        glfwGetCursorPos(window, &x, &y);
        printf("%s Maustaste bei (%f, %f) gedrückt.\n", button == GLFW_MOUSE_BUTTON_LEFT ? "Linke" : "Rechte", x, y);
    }
}

// 2D coordiantes of the corners of a square.
// Ordered for easy rendering as two triangles that share two of their corners.
const GLfloat square_corners[] = {
    0.0f, 0.0f,
    0.8f, 0.0f,
    0.0f, 0.8f,
    0.8f, 0.8f,
    };

int main(void) {
    setlocale(LC_ALL, "C.UTF-8");

    // Initialize GLFW
    GLFWwindow *window;
    glfwSetErrorCallback(&error_callback);
    if (!glfwInit()) {
        return -1;
    }

    // We want an OpenGL ES 1.1 (or compatible, but I don't think
    // Khronos will ever release OpenGL ES 1.2) context.
#ifndef USE_OPENGL_1_5_INSTEAD_OF_OPENGL_ES_1_1
    glfwWindowHint(GLFW_CONTEXT_CREATION_API, GLFW_EGL_CONTEXT_API);
    glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_ES_API);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 1);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 1);
#else
    glfwWindowHint(GLFW_CLIENT_API, GLFW_OPENGL_API);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 1);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5);
#endif

    // Create a window.
    window = glfwCreateWindow(640, 480,
        "White square in otherwise black window", 0, 0);
    if (!window) {
        glfwTerminate();
        return -1;
    }
    glfwMakeContextCurrent(window);

    // Print some information on the OpenGL ES driver
    printf("Renderer: %s\n", glGetString(GL_RENDERER));
    printf("Version: %s\n", glGetString(GL_VERSION));

    glfwSetKeyCallback(window, &key_click);
    glfwSetCharCallback(window, &char_click);
    glfwSetMouseButtonCallback(window, &mouse_click);

    glEnableClientState(GL_VERTEX_ARRAY);
    while (!glfwWindowShouldClose(window)) {
        int width, height;
        glfwGetFramebufferSize(window, &width, &height);
        float ratio = (float) width / (float) height;
        glViewport(0, 0, width, height);

        // Clear the color buffer (to default black clear color)
        glClear(GL_COLOR_BUFFER_BIT);

        // Use a projection suitable for 2D graphics.
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glOrthof(-ratio, ratio, -1.f, 1.f, 1.f, -1.f);

        // No rotation, etc for now.
        glMatrixMode(GL_MODELVIEW);
        //glLoadIdentity();

        // Draw the square
        glVertexPointer(2, GL_FLOAT, 0, square_corners);
        glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);

        // Buffer swap to display the new window content.
        glFlush();
        glfwSwapBuffers(window);

        glfwPollEvents();

        if (glGetError() != GL_NO_ERROR) {
            fprintf(stderr, "Es trat ein OpenGL-Fehler auf.\n");
            break;    
        }
    }
    glfwDestroyWindow(window);
    glfwTerminate();
    return 0;
}

