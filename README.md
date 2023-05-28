This package contains set of small programs created for practice, improvement of development skills in Haskell. Important part in the creation of the programs is chatgpt4 by openai, that helped in some cases partially, providing elements of the program, in same cases created full solution upon request and few feedbacks about bugs and errors.

Use:
- build with stack: 'stack build'
- run of each program separately: 'stack run <name>', eg. 'stack run fourier'.

1. stack fallingStars - simulation of falling stars screen saver
2. stack snake        - snake game (ESC-exit, arrows-change direction)
3. stack carpet       - Sierpinski Carpet fractal evolution (ESC-exit, SPACE-next evolution step)
4. stack triangle     - Sierpinski Triangle fractal evolution (ESC-exit, SPACE-next evolution step)
5. stack fourier      - Fourier Series visualisation. Visualisation shows series of length 4 and few
                        different functions.
                        F1 - square function
                        F2 - sawtooth function
                        F3 - another sawtooth  function 
                        F4 - function chatgpt4 managed to produce
                        F5 - triangle function
                        ESC - quit                       
6. stack sponge       - simulation of Menger Sponge (ESC-exit, SPACE-next evolution step)
7. stack balls        - simulation of brownian moves. Solid spheres closed in space limited area, move and 
                        bounce from walls and from each oter. Simulation starts with one "particle", but you can add more particles. (ESC-exit, F1-add a new random particle)
8. stack ellipsoid    - 3D drawn ellipsoids. (ESC-exit, F1-add a new random ellipsoid)
9. stack gravity      - gravity engine (Euler method) with couple of examples of system evolution
                        F1 - earth and moon
                        F2 - earth and moon orbiting 2x faster
                        F3 - earth and moon 2x heavier
                        F4 - earth of mass of half moon
                        F5 - sun, mercury, venus, earth and moon
                        All distances in scale, all sizes in another common scale, all mases original.
                        (X,x,Y,y,Z,z) - rotations over given axe
                        SPACE - pause
                        Q - exit
                        0 - eye positioned on point 0,0,0
                        1..n - eye positioned on body k
10. stack maze        - maze generation, algorithm randomized DFS (ESC-exit)

Licence: MIT

Used external resources:
- texture images from https://www.solarsystemscope.com/textures/ provided under licence https://creativecommons.org/licenses/by/4.0/

