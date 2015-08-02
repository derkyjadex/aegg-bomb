#version 150

uniform mat3 cam;

in vec2 vertexCoord;
in vec3 vertexColor;

out vec3 color;

void main() {
     color = vertexColor;
     vec2 coord = 0.03 * vertexCoord + 0.5;
     gl_Position = vec4(cam * (vec3(coord, 1) * 2 - 1), 1);
}
