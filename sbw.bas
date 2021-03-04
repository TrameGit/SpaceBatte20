'------------------------------------------------------------------------------
'    Very simple Classical Space Arcade Game
'
'    Copyright (C) 2020 by Paulo C. Ormonde
'
'    This program is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'
'    Contact e-mail: ormonde.paulo@gmail.com
'
'------------------------------------------------------------------------------
'
TYPE tfire
    fx AS INTEGER
    fy AS INTEGER
    status AS INTEGER '0-lost/waiting to throw  1-shooted
END TYPE

TYPE tstar
    ax AS INTEGER
    ay AS INTEGER
    status AS INTEGER '0-Active 1-Exploded
    rot AS INTEGER
    coll AS INTEGER
END TYPE

TYPE tpoint
    x AS INTEGER
    y AS INTEGER
END TYPE

DIM SHARED pa AS tpoint 'asteroid center point
DIM SHARED pb AS tpoint 'asteroid center point
DIM SHARED ps AS tpoint 'spaceship center point
DIM SHARED pshoot AS tpoint 'shoot center point
DIM SHARED explosion(8) AS LONG
DIM SHARED asteroid(8) AS LONG
DIM SHARED stars(31) AS tstar
DIM SHARED badball AS LONG
DIM SHARED fire(50) AS tfire
DIM SHARED firebad(31) AS tfire
DIM SHARED nfire AS INTEGER
DIM SHARED sp AS INTEGER 'screen position
DIM SHARED velocity AS INTEGER 'screen velocity effect
DIM SHARED space AS LONG
DIM SHARED spaceship AS LONG
DIM SHARED sndshoot AS LONG
DIM SHARED sndenv AS LONG
DIM SHARED sndexplosion AS LONG
DIM SHARED sndlevel AS LONG 'sound
DIM SHARED sndgameover AS LONG
DIM SHARED speedrot AS INTEGER 'speed rotation of asteroid
DIM SHARED speedcoll AS INTEGER 'speed explosion of asteroit
DIM SHARED speedshoot1 AS INTEGER
DIM SHARED life AS INTEGER
DIM SHARED iconship AS LONG
DIM SHARED startpage(3) AS LONG
DIM SHARED level(6) AS LONG
DIM SHARED gameover AS LONG
DIM SHARED phase AS INTEGER
DIM SHARED score AS INTEGER
DIM SHARED zig AS INTEGER



screenx = 800
screeny = 600
backcolor& = _RGB32(0, 0, 0)
velocity = 2
nfire = 0
speedrot = 0
speedcoll = 0
sp = 0 'initial screen position
life = 5 'lives per level
phase = 1
score = 0
zig = 1


_TITLE "Space Battle of Words"
SCREEN _NEWIMAGE(screenx, screeny, 32)
COLOR _RGB32(255, 255, 255), backcolor&
_FULLSCREEN _SQUAREPIXELS , _SMOOTH
CLS
_MOUSESHOW "crosshair"

space = _LOADIMAGE("space.png")
spaceship = _LOADIMAGE("spaceship.png")
iconship = _LOADIMAGE("iconship.png")
sndshoot = _SNDOPEN("laser.wav")
sndlevel = _SNDOPEN("level.mp3")
sndgameover = _SNDOPEN("gameover.mp3")
sndenv = _SNDOPEN("blippy.ogg")
sndexplosion = _SNDOPEN("explosion.ogg")

''load ateroid and explosion
FOR j% = 1 TO 7
    Filename$ = "asteroid" + _TRIM$(STR$(j%)) + ".png"
    asteroid(j%) = _LOADIMAGE(Filename$)
    Filename$ = "explode" + _TRIM$(STR$(j%)) + ".png"
    explosion(j%) = _LOADIMAGE(Filename$)
NEXT j%

RANDOMIZE TIMER
FOR k% = 1 TO 30
    stars(k%).ax = stars(k%).ax + (INT(RND * 8) + 1) * 100 - 50
    stars(k%).ay = stars(k%).ay - (INT(RND * 6) + 1) * (INT(RND * 6) + 1) * 100
    stars(k%).rot = (INT(RND * 7) + 1)
    stars(k%).coll = 1
    stars(k%).status = 0
    firebad(k%).fx = stars(k%).ax + 30
    firebad(k%).fy = stars(k%).ay + 60
    firebad(k%).status = 0
NEXT k%

_SNDLOOP sndenv 'start enviroment sound


startpage(1) = _LOADIMAGE("startpage1.png")
startpage(2) = _LOADIMAGE("startpage2.png")
level(1) = _LOADIMAGE("level1.png")
level(2) = _LOADIMAGE("level2.png")
level(3) = _LOADIMAGE("level3.png")
level(4) = _LOADIMAGE("level4.png")
level(5) = _LOADIMAGE("level5.png")
gameover = _LOADIMAGE("gameover.png")
badball = _LOADIMAGE("badball.png")

'Loop until start
'-------------------------------------------------------------------------------------------------------------------------
DO
    _PUTIMAGE (0, 0), startpage(1)
    _LIMIT 60
    _PUTIMAGE (0, 0), startpage(2)
    s$ = INKEY$
LOOP UNTIL s$ <> ""

'display png and snd  first level
_PUTIMAGE (0, 0), level(1)
_SNDPLAY (sndlevel)
_DELAY 2

'Main loop
'-------------------------------------------------------------------------------------------------------------------------
bad = 1
DO
    _LIMIT 30
    CLS
    enviroment


    'phase = 5

    DO WHILE _MOUSEINPUT
    LOOP
    _PUTIMAGE (_MOUSEX - 75, _MOUSEY - 75), spaceship

    gunshot
    c$ = INKEY$

    'moving asteroids
    FOR w% = 1 TO 30 'for 30 asteroids

        SELECT CASE phase
            CASE 1: move_asteroid_a (w%)
            CASE 2: move_asteroid_b (w%)
            CASE 3: move_asteroid_c (w%)
            CASE 4: move_badball_a (w%)
            CASE 5: move_badball_b (w%)
        END SELECT

        collision_detection (w%)
        shoot_detectinon (w%)
        show_explosion (w%)
    NEXT w%

    IF phase = 4 OR phase = 5 THEN gunshotbad

    _DISPLAY

    speeds

    IF life <= 0 THEN
        EXIT DO
    END IF

    IF score = 30 THEN
        IF phase < 5 THEN
            phase = phase + 1
            _PUTIMAGE (0, 0), level(phase)
            _SNDPLAY (sndlevel)
            _DISPLAY
            _DELAY 2
            score = 0
            '''life = life + 1
            RANDOMIZE TIMER
            FOR k% = 1 TO 30
                stars(k%).ay = 0
                stars(k%).ax = 0
                stars(k%).ax = stars(k%).ax + (INT(RND * 8) + 1) * 80 - 50
                stars(k%).ay = stars(k%).ay - (INT(RND * 6) + 1) * (INT(RND * 6) + 1) * 100
                stars(k%).rot = (INT(RND * 7) + 1)
                stars(k%).coll = 1
                stars(k%).status = 0
                firebad(k%).fx = stars(k%).ax + 30
                firebad(k%).fy = stars(k%).ay + 60
                firebad(k%).status = 0
            NEXT k%
        END IF
    END IF

    IF phase = 5 AND score = 30 THEN
        _PUTIMAGE (0, 0), space
        _SNDPLAY (sndlevel)
        COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)
        _PRINTSTRING (300, 200), "CONGRATULATIONS YOU WON!!!"

        _DISPLAY
        _DELAY 3
        _SNDSTOP sndenv
        _SNDCLOSE sndenv
        SYSTEM
    END IF


LOOP UNTIL c$ = CHR$(27)



IF life = 0 THEN
    _PUTIMAGE (0, 0), gameover
    _SNDPLAY (sndgameover)
    _DISPLAY
    _DELAY 2
END IF

_SNDSTOP sndenv
_SNDCLOSE sndenv
SYSTEM

'-------------------------------------------------------------------------------------------------------------------------
'Moving the stars
SUB enviroment
    _PUTIMAGE (0, -(600 - sp)), space
    _PUTIMAGE (0, sp), space
    sp = sp + velocity
    IF sp = 600 THEN sp = 0

    FOR f% = 1 TO life
        _PUTIMAGE (f% * 30, 30), iconship
    NEXT f%
    COLOR _RGB32(255, 255, 255), _RGB32(0, 0, 0)
    _PRINTSTRING (50, 5), "SCORE -> [" + STR$(score) + " ]"

END SUB


'-------------------------------------------------------------
SUB gunshot
    IF _MOUSEBUTTON(1) = -1 THEN 'mouse click
        ' _DELAY 0.05
        IF NOT _MOUSEBUTTON(1) = 0 THEN
            nfire = nfire + 1
            IF nfire <= 50 THEN
                fire(nfire).status = 1
                fire(nfire).fx = _MOUSEX
                fire(nfire).fy = _MOUSEY - 75
                _SNDPLAY sndshoot
            END IF
        END IF
        IF nfire > 50 THEN nfire = 0 'reload munition
    END IF

    FOR i% = 0 TO 50
        IF fire(i%).status = 1 THEN
            fire(i%).fy = fire(i%).fy - 10
            CIRCLE (fire(i%).fx, fire(i%).fy), 5, _RGB32(255, 55 + 4 * i%, 0)
            PAINT STEP(0, 0), _RGB32(255, 55 + 4 * i%, 0), _RGB32(255, 55 + 4 * i%, 0)
            IF fire(i%).fy <= 0 THEN
                fire(i%).status = 0
            END IF
        END IF
    NEXT i%

END SUB


'-------------------------------------------------------------

SUB gunshotbad
    FOR w% = 1 TO 30

        firebad(w%).fy = firebad(w%).fy + 10

        IF stars(w%).status = 1 AND firebad(w%).fy > 0 AND firebad(w%).status = 1 THEN
            firebad(w%).status = 1
            IF firebad(w%).fy > screeny THEN firebad(w%).status = 0
        END IF


        IF stars(w%).status = 1 AND firebad(w%).status <> 1 THEN
            firebad(w%).status = 0
        END IF

        IF stars(w%).ay < 0 THEN firebad(w%).status = 0



        IF stars(w%).status = 0 AND stars(w%).ay > 0 AND firebad(w%).status <> 1 THEN
            firebad(w%).status = 1
            firebad(w%).fy = stars(w%).ay + 60
        END IF

        IF firebad(w%).status = 1 THEN
            CIRCLE (firebad(w%).fx, firebad(w%).fy), 4, _RGB32(0, 255, 0)
            PAINT STEP(0, 0), _RGB32(0, 255, 0), _RGB32(0, 255, 0)
        END IF
    NEXT w%
END SUB



'-------------------------------------------------------------

FUNCTION distance (pj AS tpoint, pk AS tpoint)
    distance = SQR((pk.x - pj.x) ^ 2 + (pk.y - pj.y) ^ 2)
END FUNCTION


'-------------------------------------------------------------
SUB show_explosion (w%)
    IF stars(w%).status = 1 THEN
        IF speedcoll = 5 AND stars(w%).coll < 7 THEN
            stars(w%).coll = stars(w%).coll + 1
            IF stars(w%).coll > 7 THEN stars(w%).coll = 7
            _PUTIMAGE (stars(w%).ax, stars(w%).ay - 20), explosion(stars(w%).coll)
        END IF

        IF stars(w%).coll = 7 THEN 'when the last image is showing
            stars(w%).status = 2
        END IF
    END IF
END SUB


'-------------------------------------------------------------
SUB collision_detection (w%)
    pa.x = stars(w%).ax + 30
    pa.y = stars(w%).ay + 30
    pb.x = firebad(w%).fx
    pb.y = firebad(w%).fy
    ps.x = _MOUSEX
    ps.y = _MOUSEY

    IF distance(pa, ps) <= 120 THEN
        IF stars(w%).status = 0 THEN
            _SNDPLAY sndexplosion
            hitship
            stars(w%).status = 1
            score = score + 1
            life = life - 1
        END IF
    END IF

    IF distance(pb, ps) <= 100 THEN
        IF firebad(w%).status = 1 THEN
            _SNDPLAY sndexplosion
            hitship
            firebad(w%).status = 0
            life = life - 1
        END IF
    END IF
END SUB


'hit ship visual effect
'-------------------------------------------------------------
SUB hitship
    kex% = -1
    FOR ex% = 10 TO 60 STEP 10
        kex% = -1 * kex%
        CIRCLE (_MOUSEX - kex% * ex%, _MOUSEY - kex% * ex%), ex%, _RGB32(255, 255, 0)
        PAINT STEP(0, 0), _RGB32(255, 255, 0), _RGB32(255, 255, 0)
        CIRCLE (_MOUSEX + kex% * ex%, _MOUSEY + kex% * ex%), ex%, _RGB32(255, 255, 0)
        PAINT STEP(0, 0), _RGB32(255, 255, 0), _RGB32(255, 255, 0)

        _DISPLAY
        _DELAY 0.01
    NEXT ex%
    kex% = -1
    FOR ex% = 5 TO 30 STEP 5
        CIRCLE (_MOUSEX - kex% * ex%, _MOUSEY - kex% * ex%), ex%, _RGB32(255, 0, 0)
        PAINT STEP(0, 0), _RGB32(255, 0, 0), _RGB32(255, 0, 0)
        CIRCLE (_MOUSEX + kex% * ex%, _MOUSEY + kex% * ex%), ex%, _RGB32(255, 0, 0)
        PAINT STEP(0, 0), _RGB32(255, 0, 0), _RGB32(255, 0, 0)

        _DISPLAY
        _DELAY 0.01
    NEXT ex%
END SUB





'-------------------------------------------------------------
SUB shoot_detectinon (w%)
    pa.x = stars(w%).ax + 30
    pa.y = stars(w%).ay + 30

    FOR i% = 1 TO 50
        IF fire(i%).status = 1 THEN
            pshoot.x = fire(i%).fx
            pshoot.y = fire(i%).fy

            IF distance(pa, pshoot) <= 42 THEN
                IF stars(w%).status = 0 THEN
                    _SNDPLAY sndexplosion
                    stars(w%).status = 1
                    score = score + 1
                    fire(i%).status = 0
                END IF
            END IF
        END IF
    NEXT i%
END SUB

'movement of asteroids at level 1
'-------------------------------------------------------------
SUB move_asteroid_a (w%)
    stars(w%).ay = stars(w%).ay + 4
    IF stars(w%).ay > 600 THEN stars(w%).ay = -600

    IF speedrot = 5 THEN
        stars(w%).rot = stars(w%).rot + 1
        IF stars(w%).rot > 7 THEN stars(w%).rot = 1
    END IF

    IF stars(w%).status = 0 THEN
        _PUTIMAGE (stars(w%).ax, stars(w%).ay), asteroid(stars(w%).rot)
    END IF
END SUB

'-------------------------------------------------------------
SUB move_asteroid_b (w%)
    stars(w%).ay = stars(w%).ay + 6
    stars(w%).ax = stars(w%).ax + 2
    IF stars(w%).ay > 600 THEN stars(w%).ay = -600
    IF stars(w%).ax > 800 THEN stars(w%).ax = 0

    IF speedrot = 5 THEN
        stars(w%).rot = stars(w%).rot + 1
        IF stars(w%).rot > 7 THEN stars(w%).rot = 1
    END IF

    IF stars(w%).status = 0 THEN
        _PUTIMAGE (stars(w%).ax, stars(w%).ay), asteroid(stars(w%).rot)
    END IF
END SUB


'-------------------------------------------------------------
SUB move_asteroid_c (w%)
    stars(w%).ay = stars(w%).ay + 8
    stars(w%).ax = stars(w%).ax + 10 * zig
    IF stars(w%).ay > 600 THEN stars(w%).ay = -600
    IF stars(w%).ax > 800 THEN stars(w%).ax = 740

    IF speedrot = 5 THEN
        stars(w%).rot = stars(w%).rot + 1
        IF stars(w%).rot > 7 THEN stars(w%).rot = 1
    END IF

    IF stars(w%).status = 0 THEN
        _PUTIMAGE (stars(w%).ax, stars(w%).ay), asteroid(stars(w%).rot)
    END IF
END SUB

'-------------------------------------------------------------
SUB move_badball_a (w%)
    stars(w%).ay = stars(w%).ay + 4


    IF stars(w%).ay > 600 THEN
        stars(w%).ay = -600
        firebad(w%).fy = stars(w%).ay + 60
    END IF

    IF stars(w%).status = 0 THEN
        _PUTIMAGE (stars(w%).ax, stars(w%).ay), badball
    END IF

END SUB


'-------------------------------------------------------------
SUB move_badball_b (w%)
    stars(w%).ay = stars(w%).ay + 4
    stars(w%).ax = stars(w%).ax + 2

    IF stars(w%).ay > 600 THEN
        stars(w%).ay = -600
        firebad(w%).fy = stars(w%).ay + 60
        firebad(w%).fx = stars(w%).ax
    END IF

    IF stars(w%).ax > 800 THEN stars(w%).ax = 0

    IF stars(w%).status = 0 THEN
        _PUTIMAGE (stars(w%).ax, stars(w%).ay), badball
    END IF

END SUB



'control speed animation of the asteroids and their explosions
'-------------------------------------------------------------
SUB speeds
    'control speed animation of the asteroids and their explosions
    speedrot = speedrot + 1
    IF speedrot > 5 THEN speedrot = 0

    'zig-zag time phase
    speedcoll = speedcoll + 1
    IF speedcoll > 5 THEN
        speedcoll = 0
        zig = -1 * zig
    END IF

    'speed shoot of phase 4
    speedshoot1 = speedshoo1 + 1
    IF speedshoot1 > 5 THEN speedshoot1 = 0

END SUB





