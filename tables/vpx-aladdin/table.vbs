' _____                                              _____ 
'( ___ )--------------------------------------------( ___ )
' |   |                                              |   | 
' |   |     _    _        _    ____  ____ ___ _   _  |   | 
' |   |    / \  | |      / \  |  _ \|  _ \_ _| \ | | |   | 
' |   |   / _ \ | |     / _ \ | | | | | | | ||  \| | |   | 
' |   |  / ___ \| |___ / ___ \| |_| | |_| | || |\  | |   | 
' |   | /_/   \_\_____/_/   \_\____/|____/___|_| \_| |   | 
' |___|                                              |___| 
'(_____)---------------By Tombg---------------------(_____)
'
' ****************************************************************
'                       VISUAL PINBALL X
' ****************************************************************
SetLocale 1033
Randomize
'***********************************
' // User Settings in F12 menu //   
'***********************************
Dim ModeChangeBallActive
Dim ModeFlipGlowActive
Dim ModeDifficulty
Dim CheatStoryLevel : CheatStoryLevel = 0 
Dim RailChoice: RailChoice = Cabinet

'----- VR Room -----
Dim VRRoomChoice : VRRoomChoice = 3			  ' 1 - Cab Only, 2 - Minimal Room, 3 - MEGA room
Dim VRTest : VRTest = False

Sub Table1_OptionEvent(ByVal eventId)
	' -- pause processes not needed to run --
    If eventId = 1 Then DisableStaticPreRendering = True
	DMDTimer.Enabled = False ' stop FlexDMD timer
'	Controller.Pause = True

	ModeChangeBallActive = Table1.Option("ChangeColorBall", 0, 1, 1, 0, 0, Array("Yes","No"))
	ModeFlipGlowActive = Table1.Option("FlipGlow Active", 0, 1, 1, 0, 0, Array("Yes","No"))
	SetFlipGlow ModeFlipGlowActive
	ModeDifficulty = Table1.Option("Mode Easy", 0, 1, 1, 0, 0, Array("Yes","No"))
	SetDifficulty ModeDifficulty

	' -- start paused processes --
	DMDTimer.Enabled = True
'	Controller.Pause = False
    If eventId = 3 Then DisableStaticPreRendering = False

    ' VRRoom
	VRRoomChoice = Table1.Option("VR Room", 1, 3, 1, 3, 0, Array("CabOnly", "Minimal", "MEGA"))
	LoadVRRoom

	' StoryLevelselect
	CheatStoryLevel = Table1.Option("Cheat StoryLevel", 1, 6, 1, 1, 0, Array("NoCheat", "Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))
	LoadStoryLevel

    RailChoice = Table1.Option("Rails Visible", 0, 1, 1, 1, 0, Array("Cabinet", "SideRails (Default)"))
	SetRails RailChoice
    If eventId = 3 And dspTriggered Then dspTriggered = False : DisableStaticPreRendering = False : End If
	End Sub

	
Sub SetRails(Opt)
	Select Case Opt
		Case 0:
			Ramp15.Visible = 0
			Ramp16.Visible = 0
			PinCab_Blades.visible = 1
			ramp17.visible = 0
			Ramp032.Visible = 0
			Wall024.sidevisible = 0
		Case 1:
			If VRRoom = 0 Then
			Ramp15.Visible = 1
			Ramp16.Visible = 1
			ramp17.visible = 1
			End If
			PinCab_Blades.visible = 0
			Ramp032.Visible = 1
			Wall024.sidevisible = 1
	End Select
End Sub

Sub SetDifficulty(Opt)
	Select Case Opt
		Case 0:
			Primitive008.Visible = 1
			zCol_Rubber_Peg005.Collidable = 1
			Pin004.visible = 1
		Case 1:
			Primitive008.Visible = 0
			zCol_Rubber_Peg005.Collidable = 0
			Pin004.visible = 0
	End Select
End Sub

Sub SetFlipGlow(Opt)
	Select Case Opt
		Case 0:
			ChangePalmBICOL
		Case 1:
			ChangePalmOFF
	End Select
End Sub
'**************************
'DOF 
'**************************
'101 Left Flipper
'102 Right Flipper
'103 Left Slingshot
'104 Right Slingshot
'105 
'106 
'107 Bumper Right
'108 Bumper Center
'109 Bumper Left
'110 FX Left Slingshot Lighting
'111 FX Right Slingshot Lighting
'112 -Kicker001_Timer
'113 -Kicker006_Timer
'114 -Kicker004_Timer
'115 Left Flipper UP
'116 Right Flipper
'117 AutoPlunger
'118 -Beacon 'no use
'119 Shaker 'Shoot Snake
'120 AutoPlunger
'121 
'122 Knocker
'123 Ball Release
'124 -Kicker003_Timer
'125 
'126 Gear Motor 'Harem Up/Down
'127 Blower Fan 'Mega tapis And EnterCave
'128 -Kicker005_Timer
'129 KickerBackL
'130 KickerBackR
'138 FX Bumper1 Lighting
'139 FX Bumper2 Lighting
'140 FX Bumper3 Lighting
'144 
'202 FX Strobes  Lighting
'203 FX Beacon Lighting
'>300 FX

Randomize

Const BallSize = 50    ' 50 is the normal size used in the core.vbs, VP kicker routines uses this value divided by 2
Const BallMass = 1
Const SongVolume = 0.5 ' 1 is full volume. Value is from 0 to 1

' Load the core.vbs for supporting Subs and functions

On Error Resume Next
ExecuteGlobal GetTextFile("core.vbs")
If Err Then MsgBox "Can't open core.vbs"
ExecuteGlobal GetTextFile("controller.vbs")
If Err Then MsgBox "Can't open controller.vbs"
On Error Goto 0


'----- Shadow Options -----
Const DynamicBallShadowsOn = 1		'0 = no dynamic ball shadow, 1 = enable dynamic ball shadow

'----- Others Options -----
Const UltraDMDUpdateTime	= 5000	'UltraDMD update time (msec).  Increase value if you encounter stutter with UltraDMD on
Const UseUltraDMD = 0				'0 = Don't use UltraDMD, 1=Use it
Const UseBGB2S = 0					'0 = Don't use B2S, 1=Use it (0 = Displaying Multiplayer Scores on the PUPDMD)
dim usePuPDMD       : usePuPDMD=True       ' set to false to not use PuPDMD for a DMD (different that BG scoring)

' Define any Constants
Const cGameName = "Aladdin"
Const TableName = "Aladdin"
Const typefont = "Ruben"
Const numberfont = "Ruben"
Const zoomfont = "Magic School One"
Const zoombgfont = "Magic School One" ' needs to be an outline of the zoomfont
Const myVersion = "1.0.0"
Const MaxPlayers = 4     ' from 1 to 4
Const BallSaverTime = 15 ' in seconds
Const MaxMultiplier = 3  ' limit to 3x in this game, both bonus multiplier and playfield multiplier
Const BallsPerGame = 5  ' usually 3 or 5
Const MaxMultiballs = 6  ' max number of balls during multiballs

Const Special1 = 50000000  ' High score to obtain an extra ball/game
Const Special2 = 100000000
Const Special3 = 150000000

' Use FlexDMD if in FS mode

' Define Global Variables
Dim PlayersPlayingGame
Dim CurrentPlayer
Dim Credits
Dim BonusPoints(4)
Dim BonusMultiplier
Dim PlayfieldMultiplier
Dim bBonusHeld
Dim BallsRemaining(4)
Dim BallinGame(4)
Dim ExtraBallsAwards(4)
Dim Special1Awarded(4)
Dim Special2Awarded(4)
Dim Special3Awarded(4)
Dim PlayerScore(4)
Dim HighScore(4)
Dim HighScoreName(4)
Dim Tilt
Dim TiltSensitivity
Dim Tilted
Dim TotalGamesPlayed
Dim bAttractMode
Dim mBalls2Eject
Dim bAutoPlunger

' Define Game Control Variables
Dim BallsOnPlayfield
Dim BOT
Const TriggerScriptSize=10
Dim pReset(10)                 ' TriggerScriptSize
Dim pStatement(10)             ' TriggerScriptSize - holds future scripts
Dim FX

' Define Game Flags
Dim bFreePlay
Dim bGameInPlay
Dim bOnTheFirstBall(4)
Dim bBallInPlungerLane
Dim bBallSaverActive
Dim bBallSaverReady
Dim bMultiBallMode
Dim bModeGenieFree
Dim MBLock_Sav(4)
Dim	RampCount 'Bonus 2
Dim MissionCount 'Bonus 4
Dim ExtraBCount
Dim SlingCount 'Bonus 1
Dim BumperCount 'Bonus 3
Dim MBLockActive(4)
Dim KickBackActive(4)
Dim SkillShotState
Dim MysteryState(4)
Dim LampSouterrain(4)
Dim LampSoutCollected(4)
Dim bModeSouterrainActive
Dim ModeSpinnersActive(4) 'Wish Genie
Dim ModeSablier
Dim bMysterySelect
Dim bAladdinBoss
Dim SpinnerCount(4)
Dim AppleCount(4)
Dim	PieceCount(4)
Dim Snake1ok(4)
Dim Snake2ok(4)
Dim Snake3ok(4)
Dim TasmineActive 'Rouge MiSpinner NAJI
Dim TasmineCount
Dim	RaishaActive 'Violet MiAllRamp MAJI
Dim RaishaCount
Dim	NajwaActive 
Dim NajwaCount 'Rose MiBumper TAJI
Dim SultanActive 'Mi Bonus
Dim LifeBoss2count
Dim LifeAladdCount
Dim BonusSultanCount(4)
Dim JasmineCount(4) 'Bonus Coeur BG
Dim CoeurCount(4) 'Coeur PF
Dim ModeMegaTapisActive
Dim StoryCount(4)
Dim AladdinPersoState(4)
Dim JafaScoopCount
Dim bMusicOn
Dim bJustStarted
Dim bJackpot
Dim plungerIM
Dim LastSwitchHit
Dim broomdone:broomdone = 0
Dim JumpCount
Dim JackpotValue(4)

'****************************************
'		USER OPTIONS
'****************************************

Dim VolumeDial : VolumeDial = 0.8           	' Overall Mechanical sound effect volume. Recommended values should be no greater than 1.
Dim BallRollVolume : BallRollVolume = 1   	' Level of ball rolling volume. Value between 0 and 1
Dim RampRollVolume : RampRollVolume = 0.8 		' Level of ramp rolling volume. Value between 0 and 1
Dim StagedFlippers : StagedFlippers = 0         ' Staged Flippers. 0 = Disabled, 1 = Enabled

Dim TableWidth: TableWidth = Table1.Width
Dim TableHeight: TableHeight = Table1.Height
'****************************************
' core.vbs variables
Dim cbRight
Dim TopMagnet
Dim TopMagnetL
Dim TopMagnetR
'Dim POAMech
'SolCallback(22)="PoAMoveLeft"						'L_PoA (*)
'SolCallback(23)="PoAMoveRight"						'R_PoA (*)

Sub startB2S(aB2S)
'    If B2SOn Then
'        Controller.B2SSetData 1, 0
'        Controller.B2SSetData 2, 0
'        Controller.B2SSetData 3, 0
'        Controller.B2SSetData 4, 0
'        Controller.B2SSetData 5, 0
'        Controller.B2SSetData 6, 0
'        Controller.B2SSetData 7, 0
'        Controller.B2SSetData 8, 0
'        Controller.B2SSetData aB2S, 1
'    End If
End Sub

Dim UltraDMD
Sub LoadUltraDMD
    Set UltraDMD = CreateObject("UltraDMD.DMDObject")
    UltraDMD.Init
	uDMDScoreTimer.Interval = UltraDMDUpdateTime
	uDMDScoreTimer.Enabled = 1
	uDMDScoreUpdate
End Sub

Sub uDMDScoreTimer_Timer
	uDMDScoreUpdate
	pUpdateScores
End Sub

Sub uDMDScoreUpdate
	If UseUltraDMD = 1 Then
		UltraDMD.DisplayScoreboard00 PlayersPlayingGame, CurrentPlayer, PlayerScore(1), PlayerScore(2), PlayerScore(3), PlayerScore(4), "Credits " & Credits, "Ball " & FormatNumber(BallinGame(CurrentPlayer), 0) & "/" & FormatNumber(BallsPerGame, 0)
	End If
End Sub
' *********************************************************************
'                Visual Pinball Defined Script Events
' *********************************************************************

Sub Table1_Init()
    LoadEM
'	Reseths
	If usePuPDMD = true Then PUPINIT
	Dim i
	'Randomize

	'Impulse Plunger as autoplunger
    Const IMPowerSetting = 43 ' Plunger Power
    Const IMTime = 1.1        ' Time in seconds for Full Plunge
    Set plungerIM = New cvpmImpulseP
    With plungerIM
        .InitImpulseP swplunger, IMPowerSetting, IMTime
        .Random 1.5
        .InitExitSnd SoundFXDOF("mech_BallLauncher", 141, DOFPulse, DOFContactors), SoundFXDOF("fx_solenoid", 141, DOFPulse, DOFContactors)
        .CreateEvents "plungerIM"
    End With

    ' Misc. VP table objects Initialisation, droptargets, animations...
    VPObjects_Init

	' TopMagnet
    Set TopMagnet = New cvpmMagnet
    With TopMagnet
        .InitMagnet Magnet1, 35
        .GrabCenter = True
        .CreateEvents "TopMagnet"
    End With

	' TopMagnet
    Set TopMagnetL = New cvpmMagnet
    With TopMagnetL
        .InitMagnet MagnetL001, -35
        .GrabCenter = False
        .CreateEvents "TopMagnetL"
    End With

	 Set TopMagnetR = New cvpmMagnet
    With TopMagnetR
        .InitMagnet MagnetR001, -35
        .GrabCenter = False
        .CreateEvents "TopMagnetR"
    End With

    ' load saved values, highscore, names, jackpot
    Loadhs
	
    'Init main variables
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
   '     BonusMultiplier(i) = 1
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
'		bOnTheFirstBall(i) = False
    Next

	For i = 0 to 4 : bOnTheFirstBall(i) = True : Next
	' FlexDMD
'	Flex_Init
'	ShowScene flexScenes(1), FlexDMD_RenderMode_DMD_RGB, 2
	If usePuPDMD = true Then PUPINIT
	If UseUltraDMD > 0 Then LoadUltraDMD

    ' freeplay or coins
    bFreePlay = true 'we want coins

    'if bFreePlay = false Then DOF 125, DOFOn

	' Turn off the bumper lights
'	FlBumperFadeTarget(1) = 0
'	FlBumperFadeTarget(2) = 0
'	FlBumperFadeTarget(3) = 0

    ' Init main variables and any other flags
'	Dim idx
'    For idx = 1 To 4
'        bOnTheFirstBall(idx) = False ' Par défaut, pas de jeu en cours
'    Next
    bAttractMode = False
	bMysterySelect = False
	bAladdinBoss = False
    bBallInPlungerLane = False
    bBallSaverActive = False
    bBallSaverReady = False
    bGameInPlay = False
    bMusicOn = True
    BallsOnPlayfield = 0
	bMultiBallMode = False
	bAutoPlunger = False
	LastSwitchHit = ""
    Tilt = 0
    TiltSensitivity = 6
    Tilted = False
    bJustStarted = True
    ' set any lights for the attract mode
    GiOff
    StartAttractMode
	pAttractStart
	B2SGIOff
	'EndOfGame()
BSInit
End Sub

'**********************************
' 	ZMAT: General Math Functions
'**********************************
' These get used throughout the script. 

Dim PI
PI = 4 * Atn(1)

Function dSin(degrees)
	dsin = Sin(degrees * Pi / 180)
End Function

Function dCos(degrees)
	dcos = Cos(degrees * Pi / 180)
End Function

Function Atn2(dy, dx)
	If dx > 0 Then
		Atn2 = Atn(dy / dx)
	ElseIf dx < 0 Then
		If dy = 0 Then
			Atn2 = pi
		Else
			Atn2 = Sgn(dy) * (pi - Atn(Abs(dy / dx)))
		End If
	ElseIf dx = 0 Then
		If dy = 0 Then
			Atn2 = 0
		Else
			Atn2 = Sgn(dy) * pi / 2
		End If
	End If
End Function

Function ArcCos(x)
	If x = 1 Then
		ArcCos = 0/180*PI
	ElseIf x = -1 Then
		ArcCos = 180/180*PI
	Else
		ArcCos = Atn(-x/Sqr(-x * x + 1)) + 2 * Atn(1)
	End If
End Function

Function max(a,b)
	If a > b Then
		max = a
	Else
		max = b
	End If
End Function

Function min(a,b)
	If a > b Then
		min = b
	Else
		min = a
	End If
End Function

' Used for drop targets
Function InRect(px,py,ax,ay,bx,by,cx,cy,dx,dy) 'Determines if a Points (px,py) is inside a 4 point polygon A-D in Clockwise/CCW order
	Dim AB, BC, CD, DA
	AB = (bx * py) - (by * px) - (ax * py) + (ay * px) + (ax * by) - (ay * bx)
	BC = (cx * py) - (cy * px) - (bx * py) + (by * px) + (bx * cy) - (by * cx)
	CD = (dx * py) - (dy * px) - (cx * py) + (cy * px) + (cx * dy) - (cy * dx)
	DA = (ax * py) - (ay * px) - (dx * py) + (dy * px) + (dx * ay) - (dy * ax)
	
	If (AB <= 0 And BC <= 0 And CD <= 0 And DA <= 0) Or (AB >= 0 And BC >= 0 And CD >= 0 And DA >= 0) Then
		InRect = True
	Else
		InRect = False
	End If
End Function

Function InRotRect(ballx,bally,px,py,angle,ax,ay,bx,by,cx,cy,dx,dy)
	Dim rax,ray,rbx,rby,rcx,rcy,rdx,rdy
	Dim rotxy
	rotxy = RotPoint(ax,ay,angle)
	rax = rotxy(0) + px
	ray = rotxy(1) + py
	rotxy = RotPoint(bx,by,angle)
	rbx = rotxy(0) + px
	rby = rotxy(1) + py
	rotxy = RotPoint(cx,cy,angle)
	rcx = rotxy(0) + px
	rcy = rotxy(1) + py
	rotxy = RotPoint(dx,dy,angle)
	rdx = rotxy(0) + px
	rdy = rotxy(1) + py
	
	InRotRect = InRect(ballx,bally,rax,ray,rbx,rby,rcx,rcy,rdx,rdy)
End Function

Function RotPoint(x,y,angle)
	Dim rx, ry
	rx = x * dCos(angle) - y * dSin(angle)
	ry = x * dSin(angle) + y * dCos(angle)
	RotPoint = Array(rx,ry)
End Function


'******************************************************
' 	ZFLE:  FLEEP MECHANICAL SOUNDS
'******************************************************

' This part in the script is an entire block that is dedicated to the physics sound system.
' Various scripts and sounds that may be pretty generic and could suit other WPC systems, but the most are tailored specifically for the TOM table

' Many of the sounds in this package can be added by creating collections and adding the appropriate objects to those collections.
' Create the following new collections:
'	 Metals (all metal objects, metal walls, metal posts, metal wire guides)
'	 Apron (the apron walls and plunger wall)
'	 Walls (all wood or plastic walls)
'	 Rollovers (wire rollover triggers, star triggers, or button triggers)
'	 Targets (standup or drop targets, these are hit sounds only ... you will want to add separate dropping sounds for drop targets)
'	 Gates (plate gates)
'	 GatesWire (wire gates)
'	 Rubbers (all rubbers including posts, sleeves, pegs, and bands)
' When creating the collections, make sure "Fire events for this collection" is checked.
' You'll also need to make sure "Has Hit Event" is checked for each object placed in these collections (not necessary for gates and triggers).
' Once the collections and objects are added, the save, close, and restart VPX.
'
' Many places in the script need to be modified to include the correct sound effect subroutine calls. The tutorial videos linked below demonstrate
' how to make these updates. But in summary the following needs to be updated:
'	- Nudging, plunger, coin-in, start button sounds will be added to the keydown and keyup subs.
'	- Flipper sounds in the flipper solenoid subs. Flipper collision sounds in the flipper collide subs.
'	- Bumpers, slingshots, drain, ball release, knocker, spinner, and saucers in their respective subs
'	- Ball rolling sounds sub
'
' Tutorial videos by Apophis
' Audio : Adding Fleep Part 1					https://youtu.be/rG35JVHxtx4?si=zdN9W4cZWEyXbOz_
' Audio : Adding Fleep Part 2					https://youtu.be/dk110pWMxGo?si=2iGMImXXZ0SFKVCh
' Audio : Adding Fleep Part 3					https://youtu.be/ESXWGJZY_EI?si=6D20E2nUM-xAw7xy


'///////////////////////////////  SOUNDS PARAMETERS  //////////////////////////////
Dim GlobalSoundLevel, CoinSoundLevel, PlungerReleaseSoundLevel, PlungerPullSoundLevel, NudgeLeftSoundLevel
Dim NudgeRightSoundLevel, NudgeCenterSoundLevel, StartButtonSoundLevel, RollingSoundFactor

CoinSoundLevel = 1					  'volume level; range [0, 1]
NudgeLeftSoundLevel = 1				 'volume level; range [0, 1]
NudgeRightSoundLevel = 1				'volume level; range [0, 1]
NudgeCenterSoundLevel = 1			   'volume level; range [0, 1]
StartButtonSoundLevel = 0.1			 'volume level; range [0, 1]
PlungerReleaseSoundLevel = 0.8 '1 wjr   'volume level; range [0, 1]
PlungerPullSoundLevel = 1			   'volume level; range [0, 1]
RollingSoundFactor = 1.1 / 5

'///////////////////////-----Solenoids, Kickers and Flash Relays-----///////////////////////
Dim FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel, FlipperUpAttackLeftSoundLevel, FlipperUpAttackRightSoundLevel
Dim FlipperUpSoundLevel, FlipperDownSoundLevel, FlipperLeftHitParm, FlipperRightHitParm
Dim SlingshotSoundLevel, BumperSoundFactor, KnockerSoundLevel

FlipperUpAttackMinimumSoundLevel = 0.010		'volume level; range [0, 1]
FlipperUpAttackMaximumSoundLevel = 0.635		'volume level; range [0, 1]
FlipperUpSoundLevel = 1.0					   'volume level; range [0, 1]
FlipperDownSoundLevel = 0.45					'volume level; range [0, 1]
FlipperLeftHitParm = FlipperUpSoundLevel		'sound helper; not configurable
FlipperRightHitParm = FlipperUpSoundLevel	   'sound helper; not configurable
SlingshotSoundLevel = 0.95					  'volume level; range [0, 1]
BumperSoundFactor = 4.25						'volume multiplier; must not be zero
KnockerSoundLevel = 1						   'volume level; range [0, 1]

'///////////////////////-----Ball Drops, Bumps and Collisions-----///////////////////////
Dim RubberStrongSoundFactor, RubberWeakSoundFactor, RubberFlipperSoundFactor,BallWithBallCollisionSoundFactor
Dim BallBouncePlayfieldSoftFactor, BallBouncePlayfieldHardFactor, PlasticRampDropToPlayfieldSoundLevel, WireRampDropToPlayfieldSoundLevel, DelayedBallDropOnPlayfieldSoundLevel
Dim WallImpactSoundFactor, MetalImpactSoundFactor, SubwaySoundLevel, SubwayEntrySoundLevel, ScoopEntrySoundLevel
Dim SaucerLockSoundLevel, SaucerKickSoundLevel

BallWithBallCollisionSoundFactor = 3.2		  'volume multiplier; must not be zero
RubberStrongSoundFactor = 0.055 / 5			 'volume multiplier; must not be zero
RubberWeakSoundFactor = 0.075 / 5			   'volume multiplier; must not be zero
RubberFlipperSoundFactor = 0.075 / 5			'volume multiplier; must not be zero
BallBouncePlayfieldSoftFactor = 0.025		   'volume multiplier; must not be zero
BallBouncePlayfieldHardFactor = 0.025		   'volume multiplier; must not be zero
DelayedBallDropOnPlayfieldSoundLevel = 0.8	  'volume level; range [0, 1]
WallImpactSoundFactor = 0.075				   'volume multiplier; must not be zero
MetalImpactSoundFactor = 0.075 / 3
SaucerLockSoundLevel = 0.8
SaucerKickSoundLevel = 0.8

'///////////////////////-----Gates, Spinners, Rollovers and Targets-----///////////////////////

Dim GateSoundLevel, TargetSoundFactor, SpinnerSoundLevel, RolloverSoundLevel, DTSoundLevel

GateSoundLevel = 0.5 / 5			'volume level; range [0, 1]
TargetSoundFactor = 0.0025 * 10	 'volume multiplier; must not be zero
DTSoundLevel = 0.25				 'volume multiplier; must not be zero
RolloverSoundLevel = 0.25		   'volume level; range [0, 1]
SpinnerSoundLevel = 0.5			 'volume level; range [0, 1]

'///////////////////////-----Ball Release, Guides and Drain-----///////////////////////
Dim DrainSoundLevel, BallReleaseSoundLevel, BottomArchBallGuideSoundFactor, FlipperBallGuideSoundFactor

DrainSoundLevel = 0.8				   'volume level; range [0, 1]
BallReleaseSoundLevel = 1			   'volume level; range [0, 1]
BottomArchBallGuideSoundFactor = 0.2	'volume multiplier; must not be zero
FlipperBallGuideSoundFactor = 0.015	 'volume multiplier; must not be zero

'///////////////////////-----Loops and Lanes-----///////////////////////
Dim ArchSoundFactor
ArchSoundFactor = 0.025 / 5			 'volume multiplier; must not be zero

'/////////////////////////////  SOUND PLAYBACK FUNCTIONS  ////////////////////////////
'/////////////////////////////  POSITIONAL SOUND PLAYBACK METHODS  ////////////////////////////
' Positional sound playback methods will play a sound, depending on the X,Y position of the table element or depending on ActiveBall object position
' These are similar subroutines that are less complicated to use (e.g. simply use standard parameters for the PlaySound call)
' For surround setup - positional sound playback functions will fade between front and rear surround channels and pan between left and right channels
' For stereo setup - positional sound playback functions will only pan between left and right channels
' For mono setup - positional sound playback functions will not pan between left and right channels and will not fade between front and rear channels

' PlaySound full syntax - PlaySound(string, int loopcount, float volume, float pan, float randompitch, int pitch, bool useexisting, bool restart, float front_rear_fade)
' Note - These functions will not work (currently) for walls/slingshots as these do not feature a simple, single X,Y position
Sub PlaySoundAtLevelStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelExistingStatic(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 1, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticLoop(playsoundparams, aVol, tableobj)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelStaticRandomPitch(playsoundparams, aVol, randomPitch, tableobj)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtLevelActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLevelExistingActiveBall(playsoundparams, aVol)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0, 0, 1, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtLeveTimerActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 0, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelTimerExistingActiveBall(playsoundparams, aVol, ballvariable)
	PlaySound playsoundparams, 0, min(aVol,1) * VolumeDial, AudioPan(ballvariable), 0, 0, 1, 0, AudioFade(ballvariable)
End Sub

Sub PlaySoundAtLevelRoll(playsoundparams, aVol, pitch)
	PlaySound playsoundparams, - 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), randomPitch, 0, 0, 0, AudioFade(tableobj)
End Sub

' Previous Positional Sound Subs

'Sub PlaySoundAt(soundname, tableobj)
'	PlaySound soundname, 1, 1 * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
'End Sub

Sub PlaySoundAtVol(soundname, tableobj, aVol)
	PlaySound soundname, 1, min(aVol,1) * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub

'Sub PlaySoundAtBall(soundname)
'	PlaySoundAt soundname, ActiveBall
'End Sub

Sub PlaySoundAtBallVol (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 1, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtBallVolM (Soundname, aVol)
	PlaySound soundname, 1,min(aVol,1) * VolumeDial, AudioPan(ActiveBall), 0,0,0, 0, AudioFade(ActiveBall)
End Sub

Sub PlaySoundAtVolLoops(sound, tableobj, Vol, Loops)
	PlaySound sound, Loops, Vol * VolumeDial, AudioPan(tableobj), 0,0,0, 1, AudioFade(tableobj)
End Sub


'/////////////////////////////  GENERAL SOUND SUBROUTINES  ////////////////////////////

Sub SoundStartButton()
	PlaySound ("Start_Button"), 0, StartButtonSoundLevel, 0, 0.25
End Sub

Sub SoundNudgeLeft()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeLeftSoundLevel * VolumeDial, - 0.1, 0.25
End Sub

Sub SoundNudgeRight()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeRightSoundLevel * VolumeDial, 0.1, 0.25
End Sub

Sub SoundNudgeCenter()
	PlaySound ("Nudge_" & Int(Rnd * 2) + 1), 0, NudgeCenterSoundLevel * VolumeDial, 0, 0.25
End Sub

Sub SoundPlungerPull()
	PlaySoundAtLevelStatic ("Plunger_Pull_1"), PlungerPullSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseBall()
	PlaySoundAtLevelStatic ("Plunger_Release_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

Sub SoundPlungerReleaseNoBall()
	PlaySoundAtLevelStatic ("Plunger_Release_No_Ball"), PlungerReleaseSoundLevel, Plunger
End Sub

'/////////////////////////////  KNOCKER SOLENOID  ////////////////////////////

Sub KnockerSolenoid()
	PlaySoundAtLevelStatic SoundFX("Knocker_1",DOFKnocker), KnockerSoundLevel, KnockerPosition
End Sub

'/////////////////////////////  DRAIN SOUNDS  ////////////////////////////

Sub RandomSoundDrain(drainswitch)
	PlaySoundAtLevelStatic ("Drain_" & Int(Rnd * 11) + 1), DrainSoundLevel, drainswitch
End Sub

'/////////////////////////////  TROUGH BALL RELEASE SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBallRelease(drainswitch)
	PlaySoundAtLevelStatic SoundFX("BallRelease" & Int(Rnd * 7) + 1,DOFContactors), BallReleaseSoundLevel, drainswitch
End Sub

'/////////////////////////////  SLINGSHOT SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundSlingshotLeft(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_L" & Int(Rnd * 10) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

Sub RandomSoundSlingshotRight(sling)
	PlaySoundAtLevelStatic SoundFX("Sling_R" & Int(Rnd * 8) + 1,DOFContactors), SlingshotSoundLevel, Sling
End Sub

'/////////////////////////////  BUMPER SOLENOID SOUNDS  ////////////////////////////

Sub RandomSoundBumperTop(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Top_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperMiddle(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Middle_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

Sub RandomSoundBumperBottom(Bump)
	PlaySoundAtLevelStatic SoundFX("Bumpers_Bottom_" & Int(Rnd * 5) + 1,DOFContactors), Vol(ActiveBall) * BumperSoundFactor, Bump
End Sub

'/////////////////////////////  SPINNER SOUNDS  ////////////////////////////

Sub SoundSpinner(spinnerswitch)
	PlaySoundAtLevelStatic ("Spinner"), SpinnerSoundLevel, spinnerswitch
End Sub

'/////////////////////////////  FLIPPER BATS SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  FLIPPER BATS SOLENOID ATTACK SOUND  ////////////////////////////

Sub SoundFlipperUpAttackLeft(flipper)
	FlipperUpAttackLeftSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-L01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

Sub SoundFlipperUpAttackRight(flipper)
	FlipperUpAttackRightSoundLevel = RndNum(FlipperUpAttackMinimumSoundLevel, FlipperUpAttackMaximumSoundLevel)
	PlaySoundAtLevelStatic SoundFX("Flipper_Attack-R01",DOFFlippers), FlipperUpAttackLeftSoundLevel, flipper
End Sub

'/////////////////////////////  FLIPPER BATS SOLENOID CORE SOUND  ////////////////////////////

Sub RandomSoundFlipperUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_L0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperLeftHitParm, Flipper
End Sub

Sub RandomSoundFlipperUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_R0" & Int(Rnd * 9) + 1,DOFFlippers), FlipperRightHitParm, Flipper
End Sub

Sub RandomSoundReflipUpLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_L0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundReflipUpRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_ReFlip_R0" & Int(Rnd * 3) + 1,DOFFlippers), (RndNum(0.8, 1)) * FlipperUpSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownLeft(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Left_Down_" & Int(Rnd * 7) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

Sub RandomSoundFlipperDownRight(flipper)
	PlaySoundAtLevelStatic SoundFX("Flipper_Right_Down_" & Int(Rnd * 8) + 1,DOFFlippers), FlipperDownSoundLevel, Flipper
End Sub

'/////////////////////////////  FLIPPER BATS BALL COLLIDE SOUND  ////////////////////////////

Sub LeftFlipperCollide(parm)
	FlipperLeftHitParm = parm / 10
	If FlipperLeftHitParm > 1 Then
		FlipperLeftHitParm = 1
	End If
	FlipperLeftHitParm = FlipperUpSoundLevel * FlipperLeftHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RightFlipperCollide(parm)
	FlipperRightHitParm = parm / 10
	If FlipperRightHitParm > 1 Then
		FlipperRightHitParm = 1
	End If
	FlipperRightHitParm = FlipperUpSoundLevel * FlipperRightHitParm
	RandomSoundRubberFlipper(parm)
End Sub

Sub RandomSoundRubberFlipper(parm)
	PlaySoundAtLevelActiveBall ("Flipper_Rubber_" & Int(Rnd * 7) + 1), parm * RubberFlipperSoundFactor
End Sub

'/////////////////////////////  ROLLOVER SOUNDS  ////////////////////////////

Sub RandomSoundRollover()
	PlaySoundAtLevelActiveBall ("Rollover_" & Int(Rnd * 4) + 1), RolloverSoundLevel
End Sub

Sub Rollovers_Hit(idx)
	RandomSoundRollover
End Sub

'/////////////////////////////  VARIOUS PLAYFIELD SOUND SUBROUTINES  ////////////////////////////
'/////////////////////////////  RUBBERS AND POSTS  ////////////////////////////
'/////////////////////////////  RUBBERS - EVENTS  ////////////////////////////

Sub Rubbers_Hit(idx)
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 5 Then
		RandomSoundRubberStrong 1
	End If
	If finalspeed <= 5 Then
		RandomSoundRubberWeak()
	End If
End Sub

'/////////////////////////////  RUBBERS AND POSTS - STRONG IMPACTS  ////////////////////////////

Sub RandomSoundRubberStrong(voladj)
	Select Case Int(Rnd * 10) + 1
		Case 1
			PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 2
			PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 3
			PlaySoundAtLevelActiveBall ("Rubber_Strong_3"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 4
			PlaySoundAtLevelActiveBall ("Rubber_Strong_4"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 5
			PlaySoundAtLevelActiveBall ("Rubber_Strong_5"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 6
			PlaySoundAtLevelActiveBall ("Rubber_Strong_6"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 7
			PlaySoundAtLevelActiveBall ("Rubber_Strong_7"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 8
			PlaySoundAtLevelActiveBall ("Rubber_Strong_8"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 9
			PlaySoundAtLevelActiveBall ("Rubber_Strong_9"), Vol(ActiveBall) * RubberStrongSoundFactor * voladj
		Case 10
			PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6 * voladj
	End Select
End Sub

'/////////////////////////////  RUBBERS AND POSTS - WEAK IMPACTS  ////////////////////////////

Sub RandomSoundRubberWeak()
	PlaySoundAtLevelActiveBall ("Rubber_" & Int(Rnd * 9) + 1), Vol(ActiveBall) * RubberWeakSoundFactor
End Sub

'/////////////////////////////  WALL IMPACTS  ////////////////////////////

Sub Walls_Hit(idx)
	RandomSoundWall()
End Sub

Sub RandomSoundWall()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 5) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_1"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_2"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_5"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_7"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 5
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_9"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 4) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_3"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 4
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 3) + 1
			Case 1
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_4"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 2
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_6"), Vol(ActiveBall) * WallImpactSoundFactor
			Case 3
				PlaySoundAtLevelExistingActiveBall ("Wall_Hit_8"), Vol(ActiveBall) * WallImpactSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  METAL TOUCH SOUNDS  ////////////////////////////

Sub RandomSoundMetal()
	PlaySoundAtLevelActiveBall ("Metal_Touch_" & Int(Rnd * 13) + 1), Vol(ActiveBall) * MetalImpactSoundFactor
End Sub

'/////////////////////////////  METAL - EVENTS  ////////////////////////////

Sub Metals_Hit (idx)
	RandomSoundMetal
End Sub

Sub ShooterDiverter_collide(idx)
	RandomSoundMetal
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE  ////////////////////////////
'/////////////////////////////  BOTTOM ARCH BALL GUIDE - SOFT BOUNCES  ////////////////////////////

Sub RandomSoundBottomArchBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Bounce_" & Int(Rnd * 2) + 1), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
	If finalspeed < 6 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Bounce_Soft_1"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Medium_3"), Vol(ActiveBall) * BottomArchBallGuideSoundFactor
		End Select
	End If
End Sub

'/////////////////////////////  BOTTOM ARCH BALL GUIDE - HARD HITS  ////////////////////////////

Sub RandomSoundBottomArchBallGuideHardHit()
	PlaySoundAtLevelActiveBall ("Apron_Hard_Hit_" & Int(Rnd * 3) + 1), BottomArchBallGuideSoundFactor * 0.25
End Sub

Sub Apron_Hit (idx)
	If Abs(cor.ballvelx(ActiveBall.id) < 4) And cor.ballvely(ActiveBall.id) > 7 Then
		RandomSoundBottomArchBallGuideHardHit()
	Else
		RandomSoundBottomArchBallGuide
	End If
End Sub

'/////////////////////////////  FLIPPER BALL GUIDE  ////////////////////////////

Sub RandomSoundFlipperBallGuide()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 16 Then
		Select Case Int(Rnd * 2) + 1
			Case 1
				PlaySoundAtLevelActiveBall ("Apron_Hard_1"),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
			Case 2
				PlaySoundAtLevelActiveBall ("Apron_Hard_2"),  Vol(ActiveBall) * 0.8 * FlipperBallGuideSoundFactor
		End Select
	End If
	If finalspeed >= 6 And finalspeed <= 16 Then
		PlaySoundAtLevelActiveBall ("Apron_Medium_" & Int(Rnd * 3) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
	If finalspeed < 6 Then
		PlaySoundAtLevelActiveBall ("Apron_Soft_" & Int(Rnd * 7) + 1),  Vol(ActiveBall) * FlipperBallGuideSoundFactor
	End If
End Sub

'/////////////////////////////  TARGET HIT SOUNDS  ////////////////////////////

Sub RandomSoundTargetHitStrong()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 5,DOFTargets), Vol(ActiveBall) * 0.45 * TargetSoundFactor
End Sub

Sub RandomSoundTargetHitWeak()
	PlaySoundAtLevelActiveBall SoundFX("Target_Hit_" & Int(Rnd * 4) + 1,DOFTargets), Vol(ActiveBall) * TargetSoundFactor
End Sub

Sub PlayTargetSound()
	Dim finalspeed
	finalspeed = Sqr(ActiveBall.velx * ActiveBall.velx + ActiveBall.vely * ActiveBall.vely)
	If finalspeed > 10 Then
		RandomSoundTargetHitStrong()
		RandomSoundBallBouncePlayfieldSoft ActiveBall
	Else
		RandomSoundTargetHitWeak()
	End If
End Sub

Sub Targets_Hit (idx)
	PlayTargetSound
End Sub

'/////////////////////////////  BALL BOUNCE SOUNDS  ////////////////////////////

Sub RandomSoundBallBouncePlayfieldSoft(aBall)
	Select Case Int(Rnd * 9) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_1"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_3"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.8, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_4"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.5, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Soft_5"), volz(aBall) * BallBouncePlayfieldSoftFactor, aBall
		Case 6
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_1"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 7
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_2"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 8
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_5"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.2, aBall
		Case 9
			PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_7"), volz(aBall) * BallBouncePlayfieldSoftFactor * 0.3, aBall
	End Select
End Sub

Sub RandomSoundBallBouncePlayfieldHard(aBall)
	PlaySoundAtLevelStatic ("Ball_Bounce_Playfield_Hard_" & Int(Rnd * 7) + 1), volz(aBall) * BallBouncePlayfieldHardFactor, aBall
End Sub

'/////////////////////////////  DELAYED DROP - TO PLAYFIELD - SOUND  ////////////////////////////

Sub RandomSoundDelayedBallDropOnPlayfield(aBall)
	Select Case Int(Rnd * 5) + 1
		Case 1
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_1_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 2
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_2_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 3
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_3_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 4
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_4_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
		Case 5
			PlaySoundAtLevelStatic ("Ball_Drop_Playfield_5_Delayed"), DelayedBallDropOnPlayfieldSoundLevel, aBall
	End Select
End Sub

'/////////////////////////////  BALL GATES AND BRACKET GATES SOUNDS  ////////////////////////////

Sub SoundPlayfieldGate()
	PlaySoundAtLevelStatic ("Gate_FastTrigger_" & Int(Rnd * 2) + 1), GateSoundLevel, ActiveBall
End Sub

Sub SoundHeavyGate()
	PlaySoundAtLevelStatic ("Gate_2"), GateSoundLevel, ActiveBall
End Sub

Sub Gates_hit(idx)
	SoundHeavyGate
End Sub

Sub GatesWire_hit(idx)
	SoundPlayfieldGate
End Sub

'/////////////////////////////  LEFT LANE ENTRANCE - SOUNDS  ////////////////////////////

Sub RandomSoundLeftArch()
	PlaySoundAtLevelActiveBall ("Arch_L" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub RandomSoundRightArch()
	PlaySoundAtLevelActiveBall ("Arch_R" & Int(Rnd * 4) + 1), Vol(ActiveBall) * ArchSoundFactor
End Sub

Sub Arch1_hit()
	If ActiveBall.velx > 1 Then SoundPlayfieldGate
	StopSound "Arch_L1"
	StopSound "Arch_L2"
	StopSound "Arch_L3"
	StopSound "Arch_L4"
End Sub

Sub Arch1_unhit()
	If ActiveBall.velx <  - 8 Then
		RandomSoundRightArch
	End If
End Sub

Sub Arch2_hit()
	If ActiveBall.velx < 1 Then SoundPlayfieldGate
	StopSound "Arch_R1"
	StopSound "Arch_R2"
	StopSound "Arch_R3"
	StopSound "Arch_R4"
End Sub

Sub Arch2_unhit()
	If ActiveBall.velx > 10 Then
		RandomSoundLeftArch
	End If
End Sub

'/////////////////////////////  SAUCERS (KICKER HOLES)  ////////////////////////////

Sub SoundSaucerLock()
	PlaySoundAtLevelStatic ("Saucer_Enter_" & Int(Rnd * 2) + 1), SaucerLockSoundLevel, ActiveBall
End Sub

Sub SoundSaucerKick(scenario, saucer)
	Select Case scenario
		Case 0
			PlaySoundAtLevelStatic SoundFX("Saucer_Empty", DOFContactors), SaucerKickSoundLevel, saucer
		Case 1
			PlaySoundAtLevelStatic SoundFX("Saucer_Kick", DOFContactors), SaucerKickSoundLevel, saucer
	End Select
End Sub

'///////////////////////////  DROP TARGET HIT SOUNDS  ///////////////////////////

Sub RandomSoundDropTargetReset(obj)
	PlaySoundAtLevelStatic SoundFX("Drop_Target_Reset_" & Int(Rnd * 6) + 1,DOFContactors), 1, obj
End Sub

Sub SoundDropTargetDrop(obj)
	PlaySoundAtLevelStatic ("Drop_Target_Down_" & Int(Rnd * 6) + 1), 200, obj
End Sub

'/////////////////////////////  GI AND FLASHER RELAYS  ////////////////////////////

Const RelayFlashSoundLevel = 0.315  'volume level; range [0, 1];
Const RelayGISoundLevel = 1.05	  'volume level; range [0, 1];

Sub Sound_GI_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_GI_On"), 0.025 * RelayGISoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_GI_Off"), 0.025 * RelayGISoundLevel, obj
	End Select
End Sub

Sub Sound_Flash_Relay(toggle, obj)
	Select Case toggle
		Case 1
			PlaySoundAtLevelStatic ("Relay_Flash_On"), 0.025 * RelayFlashSoundLevel, obj
		Case 0
			PlaySoundAtLevelStatic ("Relay_Flash_Off"), 0.025 * RelayFlashSoundLevel, obj
	End Select
End Sub

'/////////////////////////////////////////////////////////////////
'					End Mechanical Sounds
'/////////////////////////////////////////////////////////////////

'******************************************************
'****  FLEEP MECHANICAL SOUNDS
'******************************************************



'******************************************************
'  ZANI: Misc Animations
'******************************************************

Sub LeftFlipper_Animate
	dim a: a = LeftFlipper.CurrentAngle
	FlipperLSh.RotZ = a
	LFLogo.RotZ = a
	glowbatleft.objrotz = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper_Animate
	dim a: a = RightFlipper.CurrentAngle
	FlipperRSh.RotZ = a
	RFLogo.RotZ = a
	glowbatright.objrotz = a
	'Add any right flipper related animations here
End Sub

Sub LeftFlipper001_Animate
	dim a: a = LeftFlipper001.CurrentAngle
	FlipperULSh.RotZ = a
	LFLogo001.RotZ = a
	'Add any left flipper related animations here
End Sub

Sub RightFlipper001_Animate
	dim a: a = RightFlipper001.CurrentAngle
	FlipperURSh.RotZ = a
	RFLogo001.RotZ = a
	'Add any right flipper related animations here
End Sub



'****************************************
' Real Time updatess using the GameTimer
'****************************************
'used for all the real time updates

Sub GameTimer_Timer
    RollingUpdate
	BSUpdate
    ' add any other real time update subs, like gates or diverters
    FlipperLSh.Rotz = LeftFlipper.CurrentAngle
    FlipperRSh.Rotz = RightFlipper.CurrentAngle
	FlipperULSh.RotZ = LeftFlipper001.CurrentAngle
	FlipperURSh.RotZ = RightFlipper001.CurrentAngle
	LFLogo.RotZ = LeftFlipper.CurrentAngle
	glowbatleft.objrotz = LeftFlipper.CurrentAngle
	GlowBatLightLeft.y = 1999 - 121.5 + LeftFlipper.CurrentAngle
	LFLogo001.RotZ = LeftFlipper001.CurrentAngle
	RFlogo.RotZ = RightFlipper.CurrentAngle
	glowbatright.objrotz = RightFlipper.CurrentAngle
	GlowBatLightRight.y =1999 - 121.5 - RightFlipper.CurrentAngle
	RFlogo001.RotZ = RightFlipper001.CurrentAngle
End Sub

'******
' Keys
'******

Sub Table1_KeyDown(ByVal Keycode)
'    If keycode = RightMagnaSave Then 'halplay
'        halgame                       'halplay
'        timer067.enabled=1            'halplay
'    End If   
'	If keycode = LeftMagnaSave And halplay=1 Then halgameOFF 'halplay

	If bMysterySelect Then
        SelectMystery(keycode)
    End If
	If bAladdinBoss Then
        AladdinMooveBoss(keycode)
    End If

    If Keycode = AddCreditKey Then
'		DOF 202, DOFPulse
        Credits = Credits + 1
		PlaySound "Coin_In_1"
		pDMDStartGame
		If B2SOn Then Controller.B2SSetscore 5, Credits
        if bFreePlay = False Then
'            DOF 125, DOFOn
            If(Tilted = False) Then
            End If
        End If
    End If
 
   If keycode = PlungerKey Then
        Plunger.Pullback
        PlaySoundAt "fx_plungerpull", plunger
        PlaySoundAt "fx_reload", plunger
		pDMDStartGame
    End If

    If hsbModeActive Then
        EnterHighScoreKey(keycode)
        Exit Sub
    End If

    ' Normal flipper action

    If bGameInPlay AND NOT Tilted Then

        If keycode = LeftTiltKey Then Nudge 90, 8:PlaySound "fx_nudge", 0, 1, -0.1, 0.25:CheckTilt
        If keycode = RightTiltKey Then Nudge 270, 8:PlaySound "fx_nudge", 0, 1, 0.1, 0.25:CheckTilt
        If keycode = CenterTiltKey Then Nudge 0, 9:PlaySound "fx_nudge", 0, 1, 1, 0.25:CheckTilt
	
'	If keycode = LeftMagnaSave Then
'	If keycode = RightMagnaSave Then
	
	If keycode = LeftFlipperKey And bModeSouterrainActive = False And bModeGenieFree <= 1 Then
		SolLFlipper True	'This would be called by the solenoid callbacks if using a ROM
		SolULFlipper True
		TapisLactive
	End If


	If keycode = RightFlipperKey And bModeSouterrainActive = False And bModeGenieFree <= 1 Then
		SolRFlipper True	'This would be called by the solenoid callbacks if using a ROM
'		SolURFlipper False
		TapisRactive
	Elseif keycode = RightFlipperKey And bModeSouterrainActive = True And bModeGenieFree <= 1 Then
'		SolRFlipper False	'This would be called by the solenoid callbacks if using a ROM
		SolURFlipper True
	End If

        If keycode = StartGameKey Then
'            If((PlayersPlayingGame <MaxPlayers) AND(bOnTheFirstBall(1) = True) ) Then
			If (PlayersPlayingGame < MaxPlayers) AND (bOnTheFirstBall(1) = True) Then

                If(bFreePlay = True) Then
                    PlayersPlayingGame = PlayersPlayingGame + 1
                    TotalGamesPlayed = TotalGamesPlayed + 1
                Else
                    If(Credits> 0) then
                        PlayersPlayingGame = PlayersPlayingGame + 1
                        TotalGamesPlayed = TotalGamesPlayed + 1
'						If B2SOn Then Controller.B2SSetscore 5, Credits
						PlaySound "Fx_Bonus1"
'						resetbackglass
                        Credits = Credits - 1
'						If B2SOn Then Controller.B2SSetscore 5, Credits
                        If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                        Else
                            ' Not Enough Credits to start a game.
                    End If
                End If
            End If
        End If
        Else ' If (GameInPlay)

            If keycode = StartGameKey Then
                If(bFreePlay = True) Then
                    If(BallsOnPlayfield = 0) Then
                        ResetForNewGame()
						UpdateMusicNow
                    End If
                Else
                    If(Credits> 0) Then
                        If(BallsOnPlayfield = 0) Then
                            Credits = Credits - 1
                            If Credits <1 And bFreePlay = False Then DOF 125, DOFOff
                            ResetForNewGame()
							UpdateMusicNow
                        End If
                    Else
                        ' Not Enough Credits to start a game.
                    End If
                End If
            End If
    End If ' If (GameInPlay)

End Sub

Sub Table1_KeyUp(ByVal keycode)

    If keycode = PlungerKey Then
        Plunger.Fire
        PlaySoundAt "fx_plunger", plunger
        If bBallInPlungerLane Then PlaySoundAt "fx_fire", plunger
    End If

    If hsbModeActive Then
        Exit Sub
    End If

    ' Table specific
'	If keycode = LeftMagnaSave Then 
'	If keycode = RightMagnaSave Then 

    If bGameInPLay AND NOT Tilted Then
	pDMDStartGame
		If keycode = LeftFlipperKey Then
			SolLFlipper False   'This would be called by the solenoid callbacks if using a ROM
			SolULFlipper False
		End If
	
		If keycode = RightFlipperKey Then
			SolRFlipper False   'This would be called by the solenoid callbacks if using a ROM
			SolURFlipper False
		End If
    End If
End Sub

'*************
' Pause Table
'*************

Sub table1_Paused
End Sub

Sub table1_unPaused
End Sub

Sub Table1_Exit
    Savehs
'    If B2SOn Then Controller.Stop
	If UseUltraDMD > 0 Then UltraDMD.CancelRendering
End Sub

'*******************************************
'	ZFLP: Flippers
'*******************************************

Const ReflipAngle = 20

' Flipper Solenoid Callbacks (these subs mimics how you would handle flippers in ROM based tables)
Sub SolLFlipper(Enabled) 'Left flipper solenoid callback
	If Enabled Then
		'PlaySoundAt SoundFXDOF("fx_flipperup", 101, DOFOn, DOFFlippers), LeftFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 101, DOFOn, DOFFlippers), LeftFlipper 'edited, added back in but removed fx_sound
		FlipperActivate LeftFlipper, LFPress
		LF.Fire  'leftflipper.rotatetoend
		RotateLaneLightsLeftUp
		
		If leftflipper.currentangle < leftflipper.endangle + ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper
		Else
			SoundFlipperUpAttackLeft LeftFlipper
			RandomSoundFlipperUpLeft LeftFlipper
		End If
	Else
		'PlaySoundAt SoundFXDOF("fx_flipperdown", 101, DOFOff, DOFFlippers), LeftFlipper 'edited, commented out
		PlaySoundAt SoundFXDOF("", 101, DOFOff, DOFFlippers), LeftFlipper 'edited, added back in but removed fx_sound
		FlipperDeActivate LeftFlipper, LFPress
		LeftFlipper.RotateToStart
		If LeftFlipper.currentangle < LeftFlipper.startAngle - 5 Then
			RandomSoundFlipperDownLeft LeftFlipper
		End If
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub


Sub SolRFlipper(Enabled) 'Right flipper solenoid callback
	If Enabled Then
		'PlaySoundAt SoundFXDOF("fx_flipperup", 102, DOFOn, DOFFlippers), RightFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 102, DOFOn, DOFFlippers), RightFlipper 'edited, added back in but removed fx_sound
		FlipperActivate RightFlipper, RFPress
		RF.Fire 'rightflipper.rotatetoend
		RotateLaneLightsRightUp
		
		If rightflipper.currentangle > rightflipper.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper
		Else
			SoundFlipperUpAttackRight RightFlipper
			RandomSoundFlipperUpRight RightFlipper
		End If
	Else
		'PlaySoundAt SoundFXDOF("fx_flipperdown", 102, DOFOff, DOFFlippers), RightFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 102, DOFOff, DOFFlippers), RightFlipper 'edited, added back in but removed fx_sound
		FlipperDeActivate RightFlipper, RFPress
		RightFlipper.RotateToStart
		If RightFlipper.currentangle > RightFlipper.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper
		End If
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolURFlipper(Enabled)
	If Enabled Then
		'PlaySoundAt SoundFXDOF("fx_flipperup", 116, DOFOn, DOFFlippers), RightFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 116, DOFOn, DOFFlippers), RightFlipper 'edited, added back in but removed fx_sound
		RightFlipper001.rotatetoend

		If RightFlipper001.currentangle > RightFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpRight RightFlipper001
		Else 
			SoundFlipperUpAttackRight RightFlipper001
			RandomSoundFlipperUpRight RightFlipper001
		End If
	Else
		'PlaySoundAt SoundFXDOF("fx_flipperdown", 116, DOFOff, DOFFlippers), RightFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 116, DOFOff, DOFFlippers), RightFlipper 'edited, added back in but removed fx_sound
		RightFlipper001.RotateToStart
		If RightFlipper001.currentangle > RightFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownRight RightFlipper001
		End If	
		FlipperRightHitParm = FlipperUpSoundLevel
	End If
End Sub

Sub SolULFlipper(Enabled)
	If Enabled Then
		'PlaySoundAt SoundFXDOF("fx_flipperup", 115, DOFOn, DOFFlippers), LeftFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 115, DOFOn, DOFFlippers), LeftFlipper 'edited, added back in but removed fx_sound
		LeftFlipper001.rotatetoend

		If LeftFlipper001.currentangle > LeftFlipper001.endangle - ReflipAngle Then
			RandomSoundReflipUpLeft LeftFlipper001
		Else 
			SoundFlipperUpAttackLeft LeftFlipper001
			RandomSoundFlipperUpLeft LeftFlipper001
		End If
	Else
		'PlaySoundAt SoundFXDOF("fx_flipperdown", 115, DOFOff, DOFFlippers), LeftFlipper 'edited, commented out original line
		PlaySoundAt SoundFXDOF("", 115, DOFOff, DOFFlippers), LeftFlipper 'edited, added back in but removed fx_sound
		LeftFlipper001.RotateToStart
		If LeftFlipper001.currentangle > LeftFlipper001.startAngle + 5 Then
			RandomSoundFlipperDownLeft LeftFlipper001
		End If	
		FlipperLeftHitParm = FlipperUpSoundLevel
	End If
End Sub

' Flipper collide subs
Sub LeftFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, LeftFlipper, LFCount, parm
	LF.ReProcessBalls ActiveBall
	LeftFlipperCollide parm
End Sub

Sub RightFlipper_Collide(parm)
	CheckLiveCatch ActiveBall, RightFlipper, RFCount, parm
	RF.ReProcessBalls ActiveBall
	RightFlipperCollide parm
End Sub

Sub LeftFlipper001_Collide(parm) 'edited, added
	LeftFlipperCollide parm
End Sub

Sub RightFlipper001_Collide(parm) 'edited, added
	RightFlipperCollide parm
End Sub


'******************************************************
'	ZNFF:  FLIPPER CORRECTIONS by nFozzy
'******************************************************
'
' There are several steps for taking advantage of nFozzy's flipper solution.  At a high level we'll need the following:
'	1. flippers with specific physics settings
'	2. custom triggers for each flipper (TriggerLF, TriggerRF)
'	3. and, special scripting
'
' TriggerLF and RF should now be 27 vp units from the flippers. In addition, 3 degrees should be added to the end angle
' when creating these triggers.
'
' RF.ReProcessBalls Activeball and LF.ReProcessBalls Activeball must be added the flipper_collide subs.
'
' A common mistake is incorrect flipper length.  A 3-inch flipper with rubbers will be about 3.125 inches long.
' This translates to about 147 vp units.  Therefore, the flipper start radius + the flipper length + the flipper end
' radius should  equal approximately 147 vp units. Another common mistake is is that sometimes the right flipper
' angle was set with a large postive value (like 238 or something). It should be using negative value (like -122).
'
' The following settings are a solid starting point for various eras of pinballs.
' |                    | EM's           | late 70's to mid 80's | mid 80's to early 90's | mid 90's and later |
' | ------------------ | -------------- | --------------------- | ---------------------- | ------------------ |
' | Mass               | 1              | 1                     | 1                      | 1                  |
' | Strength           | 500-1000 (750) | 1400-1600 (1500)      | 2000-2600              | 3200-3300 (3250)   |
' | Elasticity         | 0.88           | 0.88                  | 0.88                   | 0.88               |
' | Elasticity Falloff | 0.15           | 0.15                  | 0.15                   | 0.15               |
' | Fricition          | 0.8-0.9        | 0.9                   | 0.9                    | 0.9                |
' | Return Strength    | 0.11           | 0.09                  | 0.07                   | 0.055              |
' | Coil Ramp Up       | 2.5            | 2.5                   | 2.5                    | 2.5                |
' | Scatter Angle      | 0              | 0                     | 0                      | 0                  |
' | EOS Torque         | 0.4            | 0.4                   | 0.375                  | 0.375              |
' | EOS Torque Angle   | 4              | 4                     | 6                      | 6                  |
'

'******************************************************
' Flippers Polarity (Select appropriate sub based on era)
'******************************************************

Dim LF : Set LF = New FlipperPolarity
Dim RF : Set RF = New FlipperPolarity

InitPolarity

'*******************************************
' Early 90's and after

Sub InitPolarity()
	Dim x, a
	a = Array(LF, RF)
	For Each x In a
		x.AddPt "Ycoef", 0, RightFlipper.Y-65, 1 'disabled
		x.AddPt "Ycoef", 1, RightFlipper.Y-11, 1
		x.enabled = True
		x.TimeDelay = 60
		x.DebugOn=False ' prints some info in debugger

		x.AddPt "Polarity", 0, 0, 0
		x.AddPt "Polarity", 1, 0.05, - 5.5
		x.AddPt "Polarity", 2, 0.16, - 5.5
		x.AddPt "Polarity", 3, 0.20, - 0.75
		x.AddPt "Polarity", 4, 0.25, - 1.25
		x.AddPt "Polarity", 5, 0.3, - 1.75
		x.AddPt "Polarity", 6, 0.4, - 3.5
		x.AddPt "Polarity", 7, 0.5, - 5.25
		x.AddPt "Polarity", 8, 0.7, - 4.0
		x.AddPt "Polarity", 9, 0.75, - 3.5
		x.AddPt "Polarity", 10, 0.8, - 3.0
		x.AddPt "Polarity", 11, 0.85, - 2.5
		x.AddPt "Polarity", 12, 0.9, - 2.0
		x.AddPt "Polarity", 13, 0.95, - 1.5
		x.AddPt "Polarity", 14, 1, - 1.0
		x.AddPt "Polarity", 15, 1.05, -0.5
		x.AddPt "Polarity", 16, 1.1, 0
		x.AddPt "Polarity", 17, 1.3, 0

		x.AddPt "Velocity", 0, 0, 0.85
		x.AddPt "Velocity", 1, 0.23, 0.85
		x.AddPt "Velocity", 2, 0.27, 1
		x.AddPt "Velocity", 3, 0.3, 1
		x.AddPt "Velocity", 4, 0.35, 1
		x.AddPt "Velocity", 5, 0.6, 1 '0.982
		x.AddPt "Velocity", 6, 0.62, 1.0
		x.AddPt "Velocity", 7, 0.702, 0.968
		x.AddPt "Velocity", 8, 0.95,  0.968
		x.AddPt "Velocity", 9, 1.03,  0.945
		x.AddPt "Velocity", 10, 1.5,  0.945

	Next
	
	' SetObjects arguments: 1: name of object 2: flipper object: 3: Trigger object around flipper
	LF.SetObjects "LF", LeftFlipper, TriggerLF
	RF.SetObjects "RF", RightFlipper, TriggerRF
End Sub

'******************************************************
'  FLIPPER CORRECTION FUNCTIONS
'******************************************************

' modified 2023 by nFozzy
' Removed need for 'endpoint' objects
' Added 'createvents' type thing for TriggerLF / TriggerRF triggers.
' Removed AddPt function which complicated setup imo
' made DebugOn do something (prints some stuff in debugger)
'   Otherwise it should function exactly the same as before\
' modified 2024 by rothbauerw
' Added Reprocessballs for flipper collisions (LF.Reprocessballs Activeball and RF.Reprocessballs Activeball must be added to the flipper collide subs
' Improved handling to remove correction for backhand shots when the flipper is raised

Class FlipperPolarity
	Public DebugOn, Enabled
	Private FlipAt		'Timer variable (IE 'flip at 723,530ms...)
	Public TimeDelay		'delay before trigger turns off and polarity is disabled
	Private Flipper, FlipperStart, FlipperEnd, FlipperEndY, LR, PartialFlipCoef, FlipStartAngle
	Private Balls(20), balldata(20)
	Private Name
	
	Dim PolarityIn, PolarityOut
	Dim VelocityIn, VelocityOut
	Dim YcoefIn, YcoefOut
	Public Sub Class_Initialize
		ReDim PolarityIn(0)
		ReDim PolarityOut(0)
		ReDim VelocityIn(0)
		ReDim VelocityOut(0)
		ReDim YcoefIn(0)
		ReDim YcoefOut(0)
		Enabled = True
		TimeDelay = 50
		LR = 1
		Dim x
		For x = 0 To UBound(balls)
			balls(x) = Empty
			Set Balldata(x) = new SpoofBall
		Next
	End Sub
	
	Public Sub SetObjects(aName, aFlipper, aTrigger)
		
		If TypeName(aName) <> "String" Then MsgBox "FlipperPolarity: .SetObjects error: first argument must be a String (And name of Object). Found:" & TypeName(aName) End If
		If TypeName(aFlipper) <> "Flipper" Then MsgBox "FlipperPolarity: .SetObjects error: Second argument must be a flipper. Found:" & TypeName(aFlipper) End If
		If TypeName(aTrigger) <> "Trigger" Then MsgBox "FlipperPolarity: .SetObjects error: third argument must be a trigger. Found:" & TypeName(aTrigger) End If
		If aFlipper.EndAngle > aFlipper.StartAngle Then LR = -1 Else LR = 1 End If
		Name = aName
		Set Flipper = aFlipper
		FlipperStart = aFlipper.x
		FlipperEnd = Flipper.Length * Sin((Flipper.StartAngle / 57.295779513082320876798154814105)) + Flipper.X ' big floats for degree to rad conversion
		FlipperEndY = Flipper.Length * Cos(Flipper.StartAngle / 57.295779513082320876798154814105)*-1 + Flipper.Y
		
		Dim str
		str = "Sub " & aTrigger.name & "_Hit() : " & aName & ".AddBall ActiveBall : End Sub'"
		ExecuteGlobal(str)
		str = "Sub " & aTrigger.name & "_UnHit() : " & aName & ".PolarityCorrect ActiveBall : End Sub'"
		ExecuteGlobal(str)
		
	End Sub
	
	' Legacy: just no op
	Public Property Let EndPoint(aInput)
		
	End Property
	
	Public Sub AddPt(aChooseArray, aIDX, aX, aY) 'Index #, X position, (in) y Position (out)
		Select Case aChooseArray
			Case "Polarity"
				ShuffleArrays PolarityIn, PolarityOut, 1
				PolarityIn(aIDX) = aX
				PolarityOut(aIDX) = aY
				ShuffleArrays PolarityIn, PolarityOut, 0
			Case "Velocity"
				ShuffleArrays VelocityIn, VelocityOut, 1
				VelocityIn(aIDX) = aX
				VelocityOut(aIDX) = aY
				ShuffleArrays VelocityIn, VelocityOut, 0
			Case "Ycoef"
				ShuffleArrays YcoefIn, YcoefOut, 1
				YcoefIn(aIDX) = aX
				YcoefOut(aIDX) = aY
				ShuffleArrays YcoefIn, YcoefOut, 0
		End Select
	End Sub
	
	Public Sub AddBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If IsEmpty(balls(x)) Then
				Set balls(x) = aBall
				Exit Sub
			End If
		Next
	End Sub
	
	Private Sub RemoveBall(aBall)
		Dim x
		For x = 0 To UBound(balls)
			If TypeName(balls(x) ) = "IBall" Then
				If aBall.ID = Balls(x).ID Then
					balls(x) = Empty
					Balldata(x).Reset
				End If
			End If
		Next
	End Sub
	
	Public Sub Fire()
		Flipper.RotateToEnd
		processballs
	End Sub
	
	Public Property Get Pos 'returns % position a ball. For debug stuff.
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				pos = pSlope(Balls(x).x, FlipperStart, 0, FlipperEnd, 1)
			End If
		Next
	End Property
	
	Public Sub ProcessBalls() 'save data of balls in flipper range
		FlipAt = GameTime
		Dim x
		For x = 0 To UBound(balls)
			If Not IsEmpty(balls(x)) Then
				balldata(x).Data = balls(x)
			End If
		Next
		FlipStartAngle = Flipper.currentangle
		PartialFlipCoef = ((Flipper.StartAngle - Flipper.CurrentAngle) / (Flipper.StartAngle - Flipper.EndAngle))
		PartialFlipCoef = abs(PartialFlipCoef-1)
	End Sub

	Public Sub ReProcessBalls(aBall) 'save data of balls in flipper range
		If FlipperOn() Then
			Dim x
			For x = 0 To UBound(balls)
				If Not IsEmpty(balls(x)) Then
					if balls(x).ID = aBall.ID Then
						If isempty(balldata(x).ID) Then
							balldata(x).Data = balls(x)
						End If
					End If
				End If
			Next
		End If
	End Sub

	'Timer shutoff for polaritycorrect
	Private Function FlipperOn()
		If GameTime < FlipAt+TimeDelay Then
			FlipperOn = True
		End If
	End Function
	
	Public Sub PolarityCorrect(aBall)
		If FlipperOn() Then
			Dim tmp, BallPos, x, IDX, Ycoef, BalltoFlip, BalltoBase, NoCorrection, checkHit
			Ycoef = 1
			
			'y safety Exit
			If aBall.VelY > -8 Then 'ball going down
				RemoveBall aBall
				Exit Sub
			End If
			
			'Find balldata. BallPos = % on Flipper
			For x = 0 To UBound(Balls)
				If aBall.id = BallData(x).id And Not IsEmpty(BallData(x).id) Then
					idx = x
					BallPos = PSlope(BallData(x).x, FlipperStart, 0, FlipperEnd, 1)
					BalltoFlip = DistanceFromFlipperAngle(BallData(x).x, BallData(x).y, Flipper, FlipStartAngle)
					If ballpos > 0.65 Then  Ycoef = LinearEnvelope(BallData(x).Y, YcoefIn, YcoefOut)								'find safety coefficient 'ycoef' data
				End If
			Next
			
			If BallPos = 0 Then 'no ball data meaning the ball is entering and exiting pretty close to the same position, use current values.
				BallPos = PSlope(aBall.x, FlipperStart, 0, FlipperEnd, 1)
				If ballpos > 0.65 Then  Ycoef = LinearEnvelope(aBall.Y, YcoefIn, YcoefOut)												'find safety coefficient 'ycoef' data
				NoCorrection = 1
			Else
				checkHit = 50 + (20 * BallPos) 

				If BalltoFlip > checkHit or (PartialFlipCoef < 0.5 and BallPos > 0.22) Then
					NoCorrection = 1
				Else
					NoCorrection = 0
				End If
			End If
			
			'Velocity correction
			If Not IsEmpty(VelocityIn(0) ) Then
				Dim VelCoef
				VelCoef = LinearEnvelope(BallPos, VelocityIn, VelocityOut)
				
				'If partialflipcoef < 1 Then VelCoef = PSlope(partialflipcoef, 0, 1, 1, VelCoef)
				
				If Enabled Then aBall.Velx = aBall.Velx*VelCoef
				If Enabled Then aBall.Vely = aBall.Vely*VelCoef
			End If
			
			'Polarity Correction (optional now)
			If Not IsEmpty(PolarityIn(0) ) Then
				Dim AddX
				AddX = LinearEnvelope(BallPos, PolarityIn, PolarityOut) * LR
				
				If Enabled and NoCorrection = 0 Then aBall.VelX = aBall.VelX + 1 * (AddX*ycoef*PartialFlipcoef*VelCoef)
			End If
			If DebugOn Then debug.print "PolarityCorrect" & " " & Name & " @ " & GameTime & " " & Round(BallPos*100) & "%" & " AddX:" & Round(AddX,2) & " Vel%:" & Round(VelCoef*100)
		End If
		RemoveBall aBall
	End Sub
End Class

'******************************************************
'  FLIPPER POLARITY AND RUBBER DAMPENER SUPPORTING FUNCTIONS
'******************************************************

' Used for flipper correction and rubber dampeners
Sub ShuffleArray(ByRef aArray, byVal offset) 'shuffle 1d array
	Dim x, aCount
	aCount = 0
	ReDim a(UBound(aArray) )
	For x = 0 To UBound(aArray)		'Shuffle objects in a temp array
		If Not IsEmpty(aArray(x) ) Then
			If IsObject(aArray(x)) Then
				Set a(aCount) = aArray(x)
			Else
				a(aCount) = aArray(x)
			End If
			aCount = aCount + 1
		End If
	Next
	If offset < 0 Then offset = 0
	ReDim aArray(aCount-1+offset)		'Resize original array
	For x = 0 To aCount-1				'set objects back into original array
		If IsObject(a(x)) Then
			Set aArray(x) = a(x)
		Else
			aArray(x) = a(x)
		End If
	Next
End Sub

' Used for flipper correction and rubber dampeners
Sub ShuffleArrays(aArray1, aArray2, offset)
	ShuffleArray aArray1, offset
	ShuffleArray aArray2, offset
End Sub

' Used for flipper correction, rubber dampeners, and drop targets
Function BallSpeed(ball) 'Calculates the ball speed
	BallSpeed = Sqr(ball.VelX^2 + ball.VelY^2 + ball.VelZ^2)
End Function

' Used for flipper correction and rubber dampeners
Function PSlope(Input, X1, Y1, X2, Y2)		'Set up line via two points, no clamping. Input X, output Y
	Dim x, y, b, m
	x = input
	m = (Y2 - Y1) / (X2 - X1)
	b = Y2 - m*X2
	Y = M*x+b
	PSlope = Y
End Function

' Used for flipper correction
Class spoofball
	Public X, Y, Z, VelX, VelY, VelZ, ID, Mass, Radius
	Public Property Let Data(aBall)
		With aBall
			x = .x
			y = .y
			z = .z
			velx = .velx
			vely = .vely
			velz = .velz
			id = .ID
			mass = .mass
			radius = .radius
		End With
	End Property
	Public Sub Reset()
		x = Empty
		y = Empty
		z = Empty
		velx = Empty
		vely = Empty
		velz = Empty
		id = Empty
		mass = Empty
		radius = Empty
	End Sub
End Class

' Used for flipper correction and rubber dampeners
Function LinearEnvelope(xInput, xKeyFrame, yLvl)
	Dim y 'Y output
	Dim L 'Line
	'find active line
	Dim ii
	For ii = 1 To UBound(xKeyFrame)
		If xInput <= xKeyFrame(ii) Then
			L = ii
			Exit For
		End If
	Next
	If xInput > xKeyFrame(UBound(xKeyFrame) ) Then L = UBound(xKeyFrame)		'catch line overrun
	Y = pSlope(xInput, xKeyFrame(L-1), yLvl(L-1), xKeyFrame(L), yLvl(L) )
	
	If xInput <= xKeyFrame(LBound(xKeyFrame) ) Then Y = yLvl(LBound(xKeyFrame) )		 'Clamp lower
	If xInput >= xKeyFrame(UBound(xKeyFrame) ) Then Y = yLvl(UBound(xKeyFrame) )		'Clamp upper
	
	LinearEnvelope = Y
End Function

'******************************************************
'  FLIPPER TRICKS
'******************************************************
' To add the flipper tricks you must
'	 - Include a call to FlipperCradleCollision from within OnBallBallCollision subroutine
'	 - Include a call the CheckLiveCatch from the LeftFlipper_Collide and RightFlipper_Collide subroutines
'	 - Include FlipperActivate and FlipperDeactivate in the Flipper solenoid subs

RightFlipper.timerinterval = 1
Rightflipper.timerenabled = True

Sub RightFlipper_timer()
	FlipperTricks LeftFlipper, LFPress, LFCount, LFEndAngle, LFState
	FlipperTricks RightFlipper, RFPress, RFCount, RFEndAngle, RFState
	FlipperNudge RightFlipper, RFEndAngle, RFEOSNudge, LeftFlipper, LFEndAngle
	FlipperNudge LeftFlipper, LFEndAngle, LFEOSNudge,  RightFlipper, RFEndAngle
End Sub

Dim LFEOSNudge, RFEOSNudge

Sub FlipperNudge(Flipper1, Endangle1, EOSNudge1, Flipper2, EndAngle2)
	Dim b
	Dim BOT
	BOT = GetBalls
	
	If Flipper1.currentangle = Endangle1 And EOSNudge1 <> 1 Then
		EOSNudge1 = 1
		'   debug.print Flipper1.currentangle &" = "& Endangle1 &"--"& Flipper2.currentangle &" = "& EndAngle2
		If Flipper2.currentangle = EndAngle2 Then
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper1) Then
					'Debug.Print "ball in flip1. exit"
					Exit Sub
				End If
			Next
			For b = 0 To UBound(BOT)
				If FlipperTrigger(BOT(b).x, BOT(b).y, Flipper2) Then
					BOT(b).velx = BOT(b).velx / 1.3
					BOT(b).vely = BOT(b).vely - 0.5
				End If
			Next
		End If
	Else
		If Abs(Flipper1.currentangle) > Abs(EndAngle1) + 30 Then EOSNudge1 = 0
	End If
End Sub


Dim FCCDamping: FCCDamping = 0.4

Sub FlipperCradleCollision(ball1, ball2, velocity)
	if velocity < 0.7 then exit sub		'filter out gentle collisions
    Dim DoDamping, coef
    DoDamping = false
    'Check left flipper
    If LeftFlipper.currentangle = LFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, LeftFlipper) OR FlipperTrigger(ball2.x, ball2.y, LeftFlipper) Then DoDamping = true
    End If
    'Check right flipper
    If RightFlipper.currentangle = RFEndAngle Then
		If FlipperTrigger(ball1.x, ball1.y, RightFlipper) OR FlipperTrigger(ball2.x, ball2.y, RightFlipper) Then DoDamping = true
    End If
    If DoDamping Then
		coef = FCCDamping
        ball1.velx = ball1.velx * coef: ball1.vely = ball1.vely * coef: ball1.velz = ball1.velz * coef
        ball2.velx = ball2.velx * coef: ball2.vely = ball2.vely * coef: ball2.velz = ball2.velz * coef
    End If
End Sub

'*************************************************
'  Check ball distance from Flipper for Rem
'*************************************************

Function Distance(ax,ay,bx,by)
	Distance = Sqr((ax - bx) ^ 2 + (ay - by) ^ 2)
End Function

Function DistancePL(px,py,ax,ay,bx,by) 'Distance between a point and a line where point Is px,py
	DistancePL = Abs((by - ay) * px - (bx - ax) * py + bx * ay - by * ax) / Distance(ax,ay,bx,by)
End Function

Function Radians(Degrees)
	Radians = Degrees * PI / 180
End Function

Function AnglePP(ax,ay,bx,by)
	AnglePP = Atn2((by - ay),(bx - ax)) * 180 / PI
End Function

Function DistanceFromFlipper(ballx, bally, Flipper)
	DistanceFromFlipper = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Flipper.currentangle + 90)) + Flipper.x, Sin(Radians(Flipper.currentangle + 90)) + Flipper.y)
End Function

Function DistanceFromFlipperAngle(ballx, bally, Flipper, Angle)
	DistanceFromFlipperAngle = DistancePL(ballx, bally, Flipper.x, Flipper.y, Cos(Radians(Angle + 90)) + Flipper.x, Sin(Radians(angle + 90)) + Flipper.y)
End Function

Function FlipperTrigger(ballx, bally, Flipper)
	Dim DiffAngle
	DiffAngle = Abs(Flipper.currentangle - AnglePP(Flipper.x, Flipper.y, ballx, bally) - 90)
	If DiffAngle > 180 Then DiffAngle = DiffAngle - 360
	
	If DistanceFromFlipper(ballx,bally,Flipper) < 48 And DiffAngle <= 90 And Distance(ballx,bally,Flipper.x,Flipper.y) < Flipper.Length Then
		FlipperTrigger = True
	Else
		FlipperTrigger = False
	End If
End Function

'*************************************************
'  End - Check ball distance from Flipper for Rem
'*************************************************

Dim LFPress, RFPress, LFCount, RFCount
Dim LFState, RFState
Dim EOST, EOSA,Frampup, FElasticity,FReturn
Dim RFEndAngle, LFEndAngle

Const FlipperCoilRampupMode = 0 '0 = fast, 1 = medium, 2 = slow (tap passes should work)

LFState = 1
RFState = 1
EOST = leftflipper.eostorque
EOSA = leftflipper.eostorqueangle
Frampup = LeftFlipper.rampup
FElasticity = LeftFlipper.elasticity
FReturn = LeftFlipper.return
'Const EOSTnew = 1.5 'EM's to late 80's - new recommendation by rothbauerw (previously 1)
Const EOSTnew = 1.2 '90's and later - new recommendation by rothbauerw (previously 0.8)
Const EOSAnew = 1
Const EOSRampup = 0
Dim SOSRampup
Select Case FlipperCoilRampupMode
	Case 0
		SOSRampup = 2.5
	Case 1
		SOSRampup = 6
	Case 2
		SOSRampup = 8.5
End Select

Const LiveCatch = 16
Const LiveElasticity = 0.45
Const SOSEM = 0.815
'   Const EOSReturn = 0.055  'EM's
'   Const EOSReturn = 0.045  'late 70's to mid 80's
Const EOSReturn = 0.035  'mid 80's to early 90's
'   Const EOSReturn = 0.025  'mid 90's and later

LFEndAngle = Leftflipper.endangle
RFEndAngle = RightFlipper.endangle

Sub FlipperActivate(Flipper, FlipperPress)
	FlipperPress = 1
	Flipper.Elasticity = FElasticity
	
	Flipper.eostorque = EOST
	Flipper.eostorqueangle = EOSA
End Sub

Sub FlipperDeactivate(Flipper, FlipperPress)
	FlipperPress = 0
	Flipper.eostorqueangle = EOSA
	Flipper.eostorque = EOST * EOSReturn / FReturn
	
	If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 0.1 Then
		Dim b, BOT
		BOT = GetBalls
		
		For b = 0 To UBound(BOT)
			If Distance(BOT(b).x, BOT(b).y, Flipper.x, Flipper.y) < 55 Then 'check for cradle
				If BOT(b).vely >= - 0.4 Then BOT(b).vely =  - 0.4
			End If
		Next
	End If
End Sub

Sub FlipperTricks (Flipper, FlipperPress, FCount, FEndAngle, FState)
	Dim Dir
	Dir = Flipper.startangle / Abs(Flipper.startangle) '-1 for Right Flipper
	
	If Abs(Flipper.currentangle) > Abs(Flipper.startangle) - 0.05 Then
		If FState <> 1 Then
			Flipper.rampup = SOSRampup
			Flipper.endangle = FEndAngle - 3 * Dir
			Flipper.Elasticity = FElasticity * SOSEM
			FCount = 0
			FState = 1
		End If
	ElseIf Abs(Flipper.currentangle) <= Abs(Flipper.endangle) And FlipperPress = 1 Then
		If FCount = 0 Then FCount = GameTime
		
		If FState <> 2 Then
			Flipper.eostorqueangle = EOSAnew
			Flipper.eostorque = EOSTnew
			Flipper.rampup = EOSRampup
			Flipper.endangle = FEndAngle
			FState = 2
		End If
	ElseIf Abs(Flipper.currentangle) > Abs(Flipper.endangle) + 0.01 And FlipperPress = 1 Then
		If FState <> 3 Then
			Flipper.eostorque = EOST
			Flipper.eostorqueangle = EOSA
			Flipper.rampup = Frampup
			Flipper.Elasticity = FElasticity
			FState = 3
		End If
	End If
End Sub

Const LiveDistanceMin = 5  'minimum distance In vp units from flipper base live catch dampening will occur
Const LiveDistanceMax = 114 'maximum distance in vp units from flipper base live catch dampening will occur (tip protection)
Const BaseDampen = 0.55

Sub CheckLiveCatch(ball, Flipper, FCount, parm) 'Experimental new live catch
    Dim Dir, LiveDist
    Dir = Flipper.startangle / Abs(Flipper.startangle)    '-1 for Right Flipper
    Dim LiveCatchBounce   'If live catch is not perfect, it won't freeze ball totally
    Dim CatchTime
    CatchTime = GameTime - FCount
    LiveDist = Abs(Flipper.x - ball.x)

    If CatchTime <= LiveCatch And parm > 3 And LiveDist > LiveDistanceMin And LiveDist < LiveDistanceMax Then
        If CatchTime <= LiveCatch * 0.5 Then   'Perfect catch only when catch time happens in the beginning of the window
            LiveCatchBounce = 0
        Else
            LiveCatchBounce = Abs((LiveCatch / 2) - CatchTime)  'Partial catch when catch happens a bit late
        End If
        
        If LiveCatchBounce = 0 And ball.velx * Dir > 0 And LiveDist > 30 Then ball.velx = 0

        If ball.velx * Dir > 0 And LiveDist < 30 Then
            ball.velx = BaseDampen * ball.velx
            ball.vely = BaseDampen * ball.vely
            ball.angmomx = BaseDampen * ball.angmomx
            ball.angmomy = BaseDampen * ball.angmomy
            ball.angmomz = BaseDampen * ball.angmomz
        Elseif LiveDist > 30 Then
            ball.vely = LiveCatchBounce * (32 / LiveCatch) ' Multiplier for inaccuracy bounce
            ball.angmomx = 0
            ball.angmomy = 0
            ball.angmomz = 0
        End If
    Else
        If Abs(Flipper.currentangle) <= Abs(Flipper.endangle) + 1 Then FlippersD.Dampenf ActiveBall, parm
    End If
End Sub

'******************************************************
'****  END FLIPPER CORRECTIONS
'******************************************************
Sub leftInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = -abs(activeball.AngMomZ) * RndNum(3,6)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

    if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub


Sub rightInlaneSpeedLimit
	'Wylte's implementation
'    debug.print "Spin in: "& activeball.AngMomZ
'    debug.print "Speed in: "& activeball.vely
	if activeball.vely < 0 then exit sub 							'don't affect upwards movement
    activeball.AngMomZ = abs(activeball.AngMomZ) * RndNum(2,4)
    If abs(activeball.AngMomZ) > 60 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If abs(activeball.AngMomZ) > 80 Then activeball.AngMomZ = 0.8 * activeball.AngMomZ
    If activeball.AngMomZ > 100 Then activeball.AngMomZ = RndNum(80,100)
    If activeball.AngMomZ < -100 Then activeball.AngMomZ = RndNum(-80,-100)

	if abs(activeball.vely) > 5 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 10 then activeball.vely = 0.8 * activeball.vely
    if abs(activeball.vely) > 15 then activeball.vely = 0.8 * activeball.vely
    if activeball.vely > 16 then activeball.vely = RndNum(14,16)
    if activeball.vely < -16 then activeball.vely = RndNum(-14,-16)
'    debug.print "Spin out: "& activeball.AngMomZ
'    debug.print "Speed out: "& activeball.vely
End Sub
'******************************************************
' 	ZDMP:  RUBBER  DAMPENERS
'******************************************************
' These are data mined bounce curves,
' dialed in with the in-game elasticity as much as possible to prevent angle / spin issues.
' Requires tracking ballspeed to calculate COR

Sub dPosts_Hit(idx)
	RubbersD.dampen ActiveBall
	TargetBouncer ActiveBall, 1
End Sub

Sub dSleeves_Hit(idx)
	SleevesD.Dampen ActiveBall
	TargetBouncer ActiveBall, 0.7
End Sub

Sub zCol_Rubber_leftsling
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_rightsling
	RubbersD.dampen Activeball
End Sub

Sub zCol_Rubber_Peg024
	RubbersD.dampen Activeball
End Sub

Dim RubbersD				'frubber
Set RubbersD = New Dampener
RubbersD.name = "Rubbers"
RubbersD.debugOn = False	'shows info in textbox "TBPout"
RubbersD.Print = False	  'debug, reports In debugger (In vel, out cor); cor bounce curve (linear)

'for best results, try to match in-game velocity as closely as possible to the desired curve
'   RubbersD.addpoint 0, 0, 0.935   'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 0, 0, 1.1		 'point# (keep sequential), ballspeed, CoR (elasticity)
RubbersD.addpoint 1, 3.77, 0.97
RubbersD.addpoint 2, 5.76, 0.967	'dont take this as gospel. if you can data mine rubber elasticitiy, please help!
RubbersD.addpoint 3, 15.84, 0.874
RubbersD.addpoint 4, 56, 0.64	   'there's clamping so interpolate up to 56 at least

Dim SleevesD	'this is just rubber but cut down to 85%...
Set SleevesD = New Dampener
SleevesD.name = "Sleeves"
SleevesD.debugOn = False	'shows info in textbox "TBPout"
SleevesD.Print = False	  'debug, reports In debugger (In vel, out cor)
SleevesD.CopyCoef RubbersD, 0.85

'######################### Add new FlippersD Profile
'######################### Adjust these values to increase or lessen the elasticity

Dim FlippersD
Set FlippersD = New Dampener
FlippersD.name = "Flippers"
FlippersD.debugOn = False
FlippersD.Print = False
FlippersD.addpoint 0, 0, 1.1
FlippersD.addpoint 1, 3.77, 0.99
FlippersD.addpoint 2, 6, 0.99

Class Dampener
	Public Print, debugOn   'tbpOut.text
	Public name, Threshold  'Minimum threshold. Useful for Flippers, which don't have a hit threshold.
	Public ModIn, ModOut
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
	End Sub
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Dampen(aBall)
		If threshold Then
			If BallSpeed(aBall) < threshold Then Exit Sub
		End If
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If debugOn Then str = name & " In vel:" & Round(cor.ballvel(aBall.id),2 ) & vbNewLine & "desired cor: " & Round(desiredcor,4) & vbNewLine & _
		"actual cor: " & Round(realCOR,4) & vbNewLine & "ballspeed coef: " & Round(coef, 3) & vbNewLine
		If Print Then Debug.print Round(cor.ballvel(aBall.id),2) & ", " & Round(desiredcor,3)
		
		aBall.velx = aBall.velx * coef
		aBall.vely = aBall.vely * coef
		aBall.velz = aBall.velz * coef
		If debugOn Then TBPout.text = str
	End Sub
	
	Public Sub Dampenf(aBall, parm) 'Rubberizer is handle here
		Dim RealCOR, DesiredCOR, str, coef
		DesiredCor = LinearEnvelope(cor.ballvel(aBall.id), ModIn, ModOut )
		RealCOR = BallSpeed(aBall) / (cor.ballvel(aBall.id) + 0.0001)
		coef = desiredcor / realcor
		If Abs(aball.velx) < 2 And aball.vely < 0 And aball.vely >  - 3.75 Then
			aBall.velx = aBall.velx * coef
			aBall.vely = aBall.vely * coef
			aBall.velz = aBall.velz * coef
		End If
	End Sub
	
	Public Sub CopyCoef(aObj, aCoef) 'alternative addpoints, copy with coef
		Dim x
		For x = 0 To UBound(aObj.ModIn)
			addpoint x, aObj.ModIn(x), aObj.ModOut(x) * aCoef
		Next
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
End Class

'******************************************************
'  TRACK ALL BALL VELOCITIES
'  FOR RUBBER DAMPENER AND DROP TARGETS
'******************************************************

Dim cor
Set cor = New CoRTracker

Class CoRTracker
	Public ballvel, ballvelx, ballvely
	
	Private Sub Class_Initialize
		ReDim ballvel(0)
		ReDim ballvelx(0)
		ReDim ballvely(0)
	End Sub
	
	Public Sub Update()	'tracks in-ball-velocity
		Dim str, b, AllBalls, highestID
		allBalls = GetBalls
		
		For Each b In allballs
			If b.id >= HighestID Then highestID = b.id
		Next
		
		If UBound(ballvel) < highestID Then ReDim ballvel(highestID)	'set bounds
		If UBound(ballvelx) < highestID Then ReDim ballvelx(highestID)	'set bounds
		If UBound(ballvely) < highestID Then ReDim ballvely(highestID)	'set bounds
		
		For Each b In allballs
			ballvel(b.id) = BallSpeed(b)
			ballvelx(b.id) = b.velx
			ballvely(b.id) = b.vely
		Next
	End Sub
End Class

' Note, cor.update must be called in a 10 ms timer. The example table uses the GameTimer for this purpose, but sometimes a dedicated timer call RDampen is used.
'

Sub CorTimer_Timer()
	Cor.Update
End Sub

'******************************************************
'****  END PHYSICS DAMPENERS
'******************************************************

'******************************************************
' 	ZBOU: VPW TargetBouncer for targets and posts by Iaakki, Wrd1972, Apophis
'******************************************************

Const TargetBouncerEnabled = 1	  '0 = normal standup targets, 1 = bouncy targets
Const TargetBouncerFactor = 0.9	 'Level of bounces. Recommmended value of 0.7-1

Sub TargetBouncer(aBall,defvalue)
	Dim zMultiplier, vel, vratio
	If TargetBouncerEnabled = 1 And aball.z < 30 Then
		'   debug.print "velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		vel = BallSpeed(aBall)
		If aBall.velx = 0 Then vratio = 1 Else vratio = aBall.vely / aBall.velx
		Select Case Int(Rnd * 6) + 1
			Case 1
				zMultiplier = 0.2 * defvalue
			Case 2
				zMultiplier = 0.25 * defvalue
			Case 3
				zMultiplier = 0.3 * defvalue
			Case 4
				zMultiplier = 0.4 * defvalue
			Case 5
				zMultiplier = 0.45 * defvalue
			Case 6
				zMultiplier = 0.5 * defvalue
		End Select
		aBall.velz = Abs(vel * zMultiplier * TargetBouncerFactor)
		aBall.velx = Sgn(aBall.velx) * Sqr(Abs((vel ^ 2 - aBall.velz ^ 2) / (1 + vratio ^ 2)))
		aBall.vely = aBall.velx * vratio
		'   debug.print "---> velx: " & aball.velx & " vely: " & aball.vely & " velz: " & aball.velz
		'   debug.print "conservation check: " & BallSpeed(aBall)/vel
	End If
End Sub

'Add targets or posts to the TargetBounce collection if you want to activate the targetbouncer code from them
Sub TargetBounce_Hit(idx)
	TargetBouncer ActiveBall, 1
End Sub

'******************************************************
'	ZSSC: SLINGSHOT CORRECTION FUNCTIONS by apophis
'******************************************************
' To add these slingshot corrections:
'	 - On the table, add the endpoint primitives that define the two ends of the Slingshot
'	 - Initialize the SlingshotCorrection objects in InitSlingCorrection
'	 - Call the .VelocityCorrect methods from the respective _Slingshot event sub

Dim LS
Set LS = New SlingshotCorrection
Dim RS
Set RS = New SlingshotCorrection

InitSlingCorrection

Sub InitSlingCorrection
	LS.Object = LeftSlingshot
	LS.EndPoint1 = EndPoint1LS
	LS.EndPoint2 = EndPoint2LS
	
	RS.Object = RightSlingshot
	RS.EndPoint1 = EndPoint1RS
	RS.EndPoint2 = EndPoint2RS
	
	'Slingshot angle corrections (pt, BallPos in %, Angle in deg)
	' These values are best guesses. Retune them if needed based on specific table research.
	AddSlingsPt 0, 0.00, - 4
	AddSlingsPt 1, 0.45, - 7
	AddSlingsPt 2, 0.48,	0
	AddSlingsPt 3, 0.52,	0
	AddSlingsPt 4, 0.55,	7
	AddSlingsPt 5, 1.00,	4
End Sub

Sub AddSlingsPt(idx, aX, aY)		'debugger wrapper for adjusting flipper script In-game
	Dim a
	a = Array(LS, RS)
	Dim x
	For Each x In a
		x.addpoint idx, aX, aY
	Next
End Sub

Class SlingshotCorrection
	Public DebugOn, Enabled
	Private Slingshot, SlingX1, SlingX2, SlingY1, SlingY2
	
	Public ModIn, ModOut
	
	Private Sub Class_Initialize
		ReDim ModIn(0)
		ReDim Modout(0)
		Enabled = True
	End Sub
	
	Public Property Let Object(aInput)
		Set Slingshot = aInput
	End Property
	
	Public Property Let EndPoint1(aInput)
		SlingX1 = aInput.x
		SlingY1 = aInput.y
	End Property
	
	Public Property Let EndPoint2(aInput)
		SlingX2 = aInput.x
		SlingY2 = aInput.y
	End Property
	
	Public Sub AddPoint(aIdx, aX, aY)
		ShuffleArrays ModIn, ModOut, 1
		ModIn(aIDX) = aX
		ModOut(aIDX) = aY
		ShuffleArrays ModIn, ModOut, 0
		If GameTime > 100 Then Report
	End Sub
	
	Public Sub Report() 'debug, reports all coords in tbPL.text
		If Not debugOn Then Exit Sub
		Dim a1, a2
		a1 = ModIn
		a2 = ModOut
		Dim str, x
		For x = 0 To UBound(a1)
			str = str & x & ": " & Round(a1(x),4) & ", " & Round(a2(x),4) & vbNewLine
		Next
		TBPout.text = str
	End Sub
	
	
	Public Sub VelocityCorrect(aBall)
		Dim BallPos, XL, XR, YL, YR
		
		'Assign right and left end points
		If SlingX1 < SlingX2 Then
			XL = SlingX1
			YL = SlingY1
			XR = SlingX2
			YR = SlingY2
		Else
			XL = SlingX2
			YL = SlingY2
			XR = SlingX1
			YR = SlingY1
		End If
		
		'Find BallPos = % on Slingshot
		If Not IsEmpty(aBall.id) Then
			If Abs(XR - XL) > Abs(YR - YL) Then
				BallPos = PSlope(aBall.x, XL, 0, XR, 1)
			Else
				BallPos = PSlope(aBall.y, YL, 0, YR, 1)
			End If
			If BallPos < 0 Then BallPos = 0
			If BallPos > 1 Then BallPos = 1
		End If
		
		'Velocity angle correction
		If Not IsEmpty(ModIn(0) ) Then
			Dim Angle, RotVxVy
			Angle = LinearEnvelope(BallPos, ModIn, ModOut)
			'   debug.print " BallPos=" & BallPos &" Angle=" & Angle
			'   debug.print " BEFORE: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			RotVxVy = RotPoint(aBall.Velx,aBall.Vely,Angle)
			If Enabled Then aBall.Velx = RotVxVy(0)
			If Enabled Then aBall.Vely = RotVxVy(1)
			'   debug.print " AFTER: aBall.Velx=" & aBall.Velx &" aBall.Vely" & aBall.Vely
			'   debug.print " "
		End If
	End Sub
End Class


'*********
' TILT
'*********

'NOTE: The TiltDecreaseTimer Subtracts .01 from the "Tilt" variable every round

Sub CheckTilt                                    'Called when table is nudged
    Tilt = Tilt + TiltSensitivity                'Add to tilt count
    TiltDecreaseTimer.Enabled = True
    If(Tilt> TiltSensitivity) AND(Tilt <15) Then 'show a warning
		pDMDmessage1L "!Careful!",1
		pDMDLabelFadePulse "Texte1a", 2500, clRed
		if UseUltraDMD = 1 then UDMD "!CAREFUL!", "", 3000
    End if
    If Tilt> 15 Then 'If more that 15 then TILT the table
        Tilted = True
        'display Tilt
        'DMDFlush
		DOF 400, DOFPulse
		pDMDmessage1L "Tilt",1
		if UseUltraDMD = 1 then UDMD "TILT", "", 3000
        DisableTable True
        TiltRecoveryTimer.Enabled = True 'start the Tilt delay to check for all the balls to be drained
    End If
End Sub

Sub TiltDecreaseTimer_Timer
    ' DecreaseTilt
    If Tilt> 0 Then
        Tilt = Tilt - 0.1
    Else
        TiltDecreaseTimer.Enabled = False
    End If
End Sub

Sub DisableTable(Enabled)
    If Enabled Then
        'turn off GI and turn off all the lights
        GiOff
		B2SGIOff
        LightSeqTilt.Play SeqAllOff
        'Disable slings, bumpers etc
        LeftFlipper.RotateToStart
        RightFlipper.RotateToStart
        LeftSlingshot.Disabled = 1
        RightSlingshot.Disabled = 1
    Else
        'turn back on GI and the lights
        GiOn
		B2SGIOn
        LightSeqTilt.StopPlay
        LeftSlingshot.Disabled = 0
        RightSlingshot.Disabled = 0
        'clean up the buffer display
        'DMDFlush
    End If
End Sub

Sub TiltRecoveryTimer_Timer()
    ' if all the balls have been drained then..
    If(BallsOnPlayfield = 0) Then
        ' do the normal end of ball thing (this doesn't give a bonus if the table is tilted)
        EndOfBall()
        TiltRecoveryTimer.Enabled = False
    End If
' else retry (checks again in another second or so)
End Sub

'********************
' Music as wav sounds
'********************

Dim Song, UpdateMusic
Song = ""

Sub PlaySong(name)
    If bMusicOn Then
        If Song <> name Then
            StopSound Song
            Song = name
            PlaySound Song, -1, SongVolume
        End If
    End If
End Sub

Sub StopSong
    If bMusicOn Then
        StopSound Song
        Song = ""
    End If
End Sub

Sub ChangeSong
    If(BallsOnPlayfield = 0)Then
        PlaySong ""'"Mu_end"
        Exit Sub
    End If

    If bAttractMode Then
        PlaySong ""'"Mu_end"
        Exit Sub
    End If
    If bMultiBallMode Then
        PlaySong "" '"Mu_MB"
    Else
        UpdateMusicNow
    end if
End Sub

'if you add more balls to the game use changesong then if bMultiBallMode = true, your multiball song will be played.
	
'	If (keycode = PlungerKey or keycode = StartGameKey) And bOnTheFirstBall = True Then 
'		Pupevent 857
'		UpdateMusicNow
'	End If

Dim IntroTpsOn
IntroTpsOn = 0

Sub UpdateMusicNow
'	MsgBox "Debug: Player " & CurrentPlayer & " Ball1 status: " & bOnTheFirstBall(CurrentPlayer)
	If bOnTheFirstBall(CurrentPlayer) = True And usePuPDMD=True Then
		Pupevent 856
		StartFlasherApron
		IntroTpsOn = 1
	Else
    Select Case UpdateMusic
        Case 0:PlaySong "Mu_1"
        Case 1:PlaySong "Mu_2"
        Case 2:PlaySong "Mu_3"
		Case 3:PlaySong "Mu_4"
        Case 4:PlaySong "Mu_5"
        Case 5:PlaySong "Mu_1"
		Case 6:PlaySong "Mu_2"
        Case 7:PlaySong "Mu_3"
        Case 8:PlaySong "Mu_4"
		Case 9:PlaySong "Mu_5"
        Case 10:PlaySong "Mu_1"
        Case 11:PlaySong "Mu_2"
    End Select
	End If
end sub

'********************
' Play random quotes
'********************

Sub PlayQuote
    Dim tmp
    tmp = INT(RND * 123) + 1
    PlaySound "HIT_" &tmp
End Sub

'******************************************
' Change light color - simulate color leds
' changes the light color and state
' 10 colors: red, orange, amber, yellow...
'******************************************
' in this table this colors are use to keep track of the progress during the acts and battles

'colors
Dim red, orange, amber, yellow, darkgreen, green, blue, darkblue, purple, white, base, baseLB, baseLH

red = 10
orange = 9
amber = 8
yellow = 7
darkgreen = 6
green = 5
blue = 4
darkblue = 3
purple = 2
white = 1
base = 11
baseLB = 12
baseLH = 13

Sub SetLightColor(n, col, stat)
	Select Case col
		Case red
			n.color = RGB(255, 0, 0)
			n.colorfull = RGB(255, 0, 0)
'			n.color = RGB(18, 0, 0)
'			n.colorfull = RGB(255, 0, 0)
		Case orange
			n.color = RGB(255, 64, 0)
			n.colorfull = RGB(255, 64, 0)
'			n.color = RGB(18, 3, 0)
'			n.colorfull = RGB(255, 64, 0)
		Case amber
			n.color = RGB(193, 49, 0)
			n.colorfull = RGB(255, 153, 0)
		Case yellow
'			n.color = RGB(18, 18, 0)
			n.color = RGB(255, 255, 0)
			n.colorfull = RGB(255, 255, 0)
		Case darkgreen
			n.color = RGB(0, 8, 0)
			n.colorfull = RGB(0, 64, 0)
		Case green
			n.color = RGB(0, 255, 0)
'			n.color = RGB(0, 18, 0)
			n.colorfull = RGB(0, 255, 0)
		Case blue
			n.color = RGB(0, 18, 18)
			n.colorfull = RGB(0, 255, 255)
		Case darkblue
			n.color = RGB(0, 8, 8)
			n.colorfull = RGB(0, 64, 64)
		Case purple
			n.color = RGB(128, 0, 128)
			n.colorfull = RGB(255, 0, 255)
		Case white
			n.color = RGB(255, 252, 224)
			n.colorfull = RGB(193, 91, 0)
'		Case white
'			n.color = RGB(255, 252, 224)
'			n.colorfull = RGB(193, 91, 0)
		Case base
			n.color = RGB(255, 197, 143)
			n.colorfull = RGB(255, 255, 236)
		Case baseLB
			n.color = RGB(0, 0, 160)
			n.colorfull = RGB(0, 0, 160)
		Case baseLH
			n.color = RGB(34, 100, 255)
			n.colorfull = RGB(34, 100, 255)
	End Select
	If stat <> -1 Then
		n.State = 0
		n.State = stat
	End If
End Sub
'**********************
'     GI effects
' independent routine
' it turns on the gi
' when there is a ball
' in play
'**********************
'B2S GI On and Off support
Sub B2SGIOn
'	If B2SOn Then Controller.B2SSetData 1,1
End Sub
Sub B2SGIOff
'	If B2SOn Then Controller.B2SSetData 1,0
End Sub

Dim OldGiState
OldGiState = -1   'start witht the Gi off

Sub ChangeGi(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILights
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGiLogo(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILightsLogo
        SetLightColor bulb, col, -1
    Next
End Sub

Sub ChangeGiSout(col) 'changes the gi color
    Dim bulb
    For each bulb in aGILightsSout
        SetLightColor bulb, col, -1
    Next
End Sub

Sub GIUpdateTimer_Timer
    Dim tmp, obj
    tmp = Getballs
    If UBound(tmp) <> OldGiState Then
        OldGiState = Ubound(tmp)
        If UBound(tmp) = 1 Then 'we have 2 captive balls on the table (-1 means no balls, 0 is the first ball, 1 is the second..)
            GiOff               ' turn off the gi if no active balls on the table, we could also have used the variable ballsonplayfield.
        Else
            Gion
        End If
    End If
End Sub

Sub GiOn
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 1
    Next
    For each bulb in aBumperLights
        bulb.State = 1
    Next
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	TorcheB001.Visible = 1
End Sub

Sub GiOff
    Dim bulb
    For each bulb in aGiLights
        bulb.State = 0
    Next
    For each bulb in aBumperLights
        bulb.State = 0
    Next
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to3"
	TorcheB001.Visible = 0
End Sub

Sub GiSoutON
	 Dim bulb
    For each bulb in aGiLightsSout
        bulb.State = 1
    Next
End Sub
Sub GiSoutOFF
	 Dim bulb
    For each bulb in aGiLightsSout
        bulb.State = 0
    Next
End Sub
' GI, light & flashers sequence effects

Sub GiEffect(n)
    Dim ii
    Select Case n
        Case 0 'all off
            LightSeqGi.Play SeqAlloff
        Case 1 'all blink
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 10, 10
        Case 4 'all blink once
            LightSeqGi.UpdateInterval = 10
            LightSeqGi.Play SeqBlinking, , 4, 1
    End Select
End Sub

Sub LightEffect(n)
    Select Case n
        Case 0 ' all off
            LightSeqInserts.Play SeqAlloff
        Case 1 'all blink
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 15, 10
        Case 2 'random
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqRandom, 50, , 1000
        Case 3 'all blink fast
            LightSeqInserts.UpdateInterval = 10
            LightSeqInserts.Play SeqBlinking, , 10, 10
        Case 4 'up 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 1
        Case 5 'up 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqUpOn, 8, 2
        Case 6 'down 1 time
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 1
        Case 7 'down 2 times
            LightSeqInserts.UpdateInterval = 4
            LightSeqInserts.Play SeqDownOn, 8, 2
    End Select
End Sub

Sub ChangeGILogoNow
	Select Case Int(Rnd * 6) + 1
		Case 1
			ChangeGiLogo red
		Case 2
			ChangeGiLogo green
		Case 3
			ChangeGiLogo purple
		Case 4
			ChangeGiLogo blue
		Case 5
			ChangeGiLogo yellow
		Case 6
			ChangeGiLogo white
	End Select
End Sub
' *********************************************************************
'                      Supporting Ball & Sound Functions
' *********************************************************************

Function Pan(ball) ' Calculates the pan for a ball based on the X position on the table. "table1" is the name of the table
    Dim tmp
    tmp = ball.x * 2 / table1.width-1
    If tmp > 0 Then
        Pan = Csng(tmp ^10)
    Else
        Pan = Csng(-((- tmp) ^10))
    End If
End Function

Sub PlaySoundAt(soundname, tableobj) 'play sound at X and Y position of an object, mostly bumpers, flippers and other fast objects
    PlaySound soundname, 0, 1, Pan(tableobj), 0.06, 0, 0, 0, AudioFade(tableobj)
End Sub

Sub PlaySoundAtBall(soundname) ' play a sound at the ball position, like rubbers, targets, metals, plastics
    PlaySound soundname, 0, Vol(ActiveBall), pan(ActiveBall), 0.2, 0, 0, 0, AudioFade(ActiveBall)
End Sub

Function RndNbr(n) 'returns a random number between 1 and n
    Randomize timer
    RndNbr = Int((n * Rnd) + 1)
End Function
'********************************************
'   JP's VP10 Rolling Sounds
'********************************************

Const tnob = 11 ' total number of balls
Const lob = 0   'number of locked balls
ReDim rolling(tnob)
'InitRolling 'edited, added later in script

Sub InitRolling_old 'edited, renamed to _old
    Dim i
    For i = 0 to tnob
        rolling(i) = False
    Next
End Sub

Sub RollingUpdate_old() 'edited, renamed to _old

    Dim BOT, b, ballpitch, ballvol
    BOT = GetBalls

    ' stop the sound of deleted balls
    For b = UBound(BOT) + 1 to tnob
        rolling(b) = False
        StopSound("fx_ballrolling" & b)
    Next

    ' exit the sub if no balls on the table
    If UBound(BOT) = lob - 1 Then Exit Sub 'there no extra balls on this table

    ' play the rolling sound for each ball
    For b = lob to UBound(BOT)

        If BallVel(BOT(b) )> 1 Then
            If BOT(b).z <30 Then
                ballpitch = Pitch(BOT(b) )
                ballvol = Vol(BOT(b) )
            Else
                ballpitch = Pitch(BOT(b) ) + 25000 'increase the pitch on a ramp
                ballvol = Vol(BOT(b) ) * 10
            End If
            rolling(b) = True
            PlaySound("fx_ballrolling" & b), -1, ballvol, Pan(BOT(b) ), 0, ballpitch, 1, 0, AudioFade(BOT(b) )
        Else
            If rolling(b) = True Then
                StopSound("fx_ballrolling" & b)
                rolling(b) = False
            End If
        End If
        ' rothbauerw's Dropping Sounds
        If BOT(b).VelZ <-1 and BOT(b).z <55 and BOT(b).z> 27 Then 'height adjust for ball drop sounds
            PlaySound "fx_balldrop", 0, ABS(BOT(b).velz) / 17, Pan(BOT(b) ), 0, Pitch(BOT(b) ), 1, 0, AudioFade(BOT(b) )
        End If

		'Tapi on broom - Suivi de la rampe
		If broomdone = 0 Then
		' 1. On vérifie si la bille est dans le couloir (X > 920)
		' 2. On vérifie si elle est entre le bas (2190) et le haut (1005) et sous la haut du haut
		If BOT(b).X > 920 And BOT(b).Y > 1005 And BOT(b).Y < 2190 And BOT(b).Z < 120 Then
        
        ' La primitive suit la bille en Y
			Tapisbroom.Y = BOT(b).Y
        
        ' FORCE le Z à rester à 4 pour éviter qu'il ne s'envole à l'initialisation
			If BOT(b).Y < 1560 Then Tapisbroom.Z = BOT(b).Z Else Tapisbroom.Z = 4 

        ' DETECTION DU SOMMET : 
        ' Si la bille dépasse le point 1005 (elle devient plus petite que 1005)
			If BOT(b).Y <= 1110 Then 
				broomdone = 1
				Tapisbreturn.Enabled = True ' Active le timer de retour
				debug.print "Retour activé !"
			End If
		End If
End If

    Next
End Sub

'***********************
'  Tapi on launch lane
'***********************

Sub Tapisbreturn_Timer()
    Dim targetY
    targetY = 2190
    
    ' Retour de la position Y
    If Tapisbroom.Y < targetY Then
        Tapisbroom.Y = Tapisbroom.Y + 15 ' vitesse de redescente
    End If

    ' Gestion fluide du Z pendant le retour
    ' Si on est au dessus du sol, on fait descendre le tapis progressivement vers Z=4
    If Tapisbroom.Z > 4 Then
        Tapisbroom.Z = Tapisbroom.Z - 2 ' Descend de 2 unités par tic
        If Tapisbroom.Z <= 4 Then Tapisbroom.Z = 4 ' Sécurité pour ne pas s'enfoncer sous la table
    End If

    ' Condition d'arrêt finale
    If Tapisbroom.Y >= targetY Then
        Tapisbroom.Y = targetY
        Tapisbroom.Z = 4
        Tapisbreturn.Enabled = False
        broomdone = 0 
    End If
End Sub
'**********************
' Ball Collision Sound
'**********************
Sub OnBallBallCollision(ball1, ball2, velocity)
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select

	PlaySound (snd), 0, Csng(velocity) ^2 / 200 * BallWithBallCollisionSoundFactor * VolumeDial, AudioPan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
End Sub

Sub SoundOnOrbCollision()
	Dim snd
	Select Case Int(Rnd*7)+1
		Case 1 : snd = "Ball_Collide_1"
		Case 2 : snd = "Ball_Collide_2"
		Case 3 : snd = "Ball_Collide_3"
		Case 4 : snd = "Ball_Collide_4"
		Case 5 : snd = "Ball_Collide_5"
		Case 6 : snd = "Ball_Collide_6"
		Case 7 : snd = "Ball_Collide_7"
	End Select
	On error Resume Next 
	PlaySoundAtLevelActiveBall snd, Vol(ActiveBall) * BallWithBallCollisionSoundFactor
	On Error goto 0
End Sub

'Sub OnBallBallCollision(ball1, ball2, velocity)
'    PlaySound "fx_collide", 0, Csng(velocity) ^2 / 2000, Pan(ball1), 0, Pitch(ball1), 0, 0, AudioFade(ball1)
'End Sub

'******************************
' Diverse Collection Hit Sounds
'******************************

'Sub aMetals_Hit(idx):PlaySoundAtBall "fx_MetalHit":End Sub
'Sub aRubber_Bands_Hit(idx):PlaySoundAtBall "fx_rubber_band":End Sub
'Sub aRubber_Posts_Hit(idx):PlaySoundAtBall "fx_rubber_post":End Sub
'Sub aRubber_Pins_Hit(idx):PlaySoundAtBall "fx_rubber_pin":End Sub
'Sub aPlastics_Hit(idx):PlaySoundAtBall "fx_PlasticHit":End Sub
'Sub aGates_Hit(idx):PlaySoundAtBall "fx_Gate":End Sub
'Sub aWoods_Hit(idx):PlaySoundAtBall "fx_Woodhit":End Sub

'Sub RHelp1_Hit() 'edited, commented out
'    StopSound "fx_metalrolling" 
'    PlaySoundAtBall "fx_ballrampdrop" 
'End Sub 

'Sub RHelp2_Hit() 'edited, commented out
'    StopSound "fx_metalrolling" 
'    PlaySoundAtBall"fx_ballrampdrop" 
'End Sub 



' *********************************************************************
'                        User Defined Script Events
' *********************************************************************

' Initialise the Table for a new Game
'
Sub ResetForNewGame()
    Dim i

    bGameInPLay = True

    'resets the score display, and turn off attract mode
    StopAttractMode
	ResetEvents
    GiOn
	B2SGIOn

    TotalGamesPlayed = TotalGamesPlayed + 1
    CurrentPlayer = 1
    PlayersPlayingGame = 1
'    bOnTheFirstBall = True
	'Multiball=false	
    For i = 1 To MaxPlayers
        PlayerScore(i) = 0
        BonusPoints(i) = 0
		'BonusHeldPoints(i) = 0
  '      BonusMultiplier(i) = 1
		bOnTheFirstBall(i) = True
        BallsRemaining(i) = BallsPerGame
		BallinGame(i) = 1
        ExtraBallsAwards(i) = 0
        Special1Awarded(i) = False
        Special2Awarded(i) = False
        Special3Awarded(i) = False
    Next
'	If B2SOn Then Controller.B2SSetScorePlayer 1, 0

    ' initialise any other flags
    Tilt = 0

	'reset variables
	bumperHits = 100
    UpdateMusic = 0
    'UpdateMusic = UpdateMusic + 6
'    UpdateMusicNow

    ' initialise Game variables
    Game_Init()
	
    ' you may wish to start some music, play a sound, do whatever at this point
StopSong


    vpmtimer.addtimer 1500, "FirstBall '"
End Sub

' This is used to delay the start of a game to allow any attract sequence to

' complete.  When it expires it creates a ball for the player to start playing with

Sub FirstBall
	ResetEvents
'	PlaySound "Vo_Start"
    ' reset the table for a new ball
    ResetForNewPlayerBall()
    ' create a new ball in the shooters lane
    CreateNewBall()
End Sub

' (Re-)Initialise the Table for a new ball (either a new ball after the player has
' lost one or we have moved onto the next player (if multiple are playing))

Sub ResetForNewPlayerBall()
    ' make sure the correct display is upto date
    AddScore 0

    ' set the current players bonus multiplier back down to 1X
'   BonusMultiplier = 1
'	PlayfieldMultiplier = 1
    'UpdateBonusXLights
	
' reset any drop targets, lights, game Mode etc..
	CheckEventPlayer
	UpdateMusicNow
   'This is a new ball, so activate the ballsaver
    bBallSaverReady = True

    'Reset any table specific
'	BumperBonus = 0
	JackpotsBonus = 0
'	MulitballBonus = 0
    ResetNewBallVariables
    ResetNewBallLights()
	'Multiball=false	
End Sub

' Create a new ball on the Playfield

Sub CreateNewBall()
    
	LightSeqAttract.StopPlay

	' create a ball in the plunger lane kicker.
    BallRelease.CreateSizedBallWithMass BallSize / 2, BallMass

    ' There is a (or another) ball on the playfield
    BallsOnPlayfield = BallsOnPlayfield + 1

    ' kick it out..
    PlaySoundAt SoundFXDOF("", 123, DOFPulse, DOFContactors), BallRelease  'burger
	RandomSoundBallRelease ballrelease 'burger
   BallRelease.Kick 90, 4

	'only this tableDrain / Plunger Functions
	'ChangeBallImage

End Sub


' Add extra balls to the table with autoplunger
' Use it as AddMultiball 4 to add 4 extra balls to the table

Sub AddMultiball(nballs)
    mBalls2Eject = mBalls2Eject + nballs
    CreateMultiballTimer.Enabled = True
    'and eject the first ball
    CreateMultiballTimer_Timer
End Sub

' Eject the ball after the delay, AddMultiballDelay
Sub CreateMultiballTimer_Timer()
    ' wait if there is a ball in the plunger lane
    If bBallInPlungerLane Then
        Exit Sub
    Else
        If BallsOnPlayfield < MaxMultiballs Then
            CreateNewBall()
            mBalls2Eject = mBalls2Eject -1
            If mBalls2Eject = 0 Then 'if there are no more balls to eject then stop the timer
                CreateMultiballTimer.Enabled = False
            End If
        Else 'the max number of multiballs is reached, so stop the timer
            mBalls2Eject = 0
            CreateMultiballTimer.Enabled = False
        End If
    End If
End Sub


' The Player has lost his ball (there are no more balls on the playfield).
' Handle any bonus points awarded

Sub EndOfBall()
	Dim BonusDelayTime
	' the first ball has been lost. From this point on no new players can join in
    bOnTheFirstBall(CurrentPlayer) = False

    ' only process any of this if the table is not tilted.  (the tilt recovery
    ' mechanism will handle any extra balls or end of game)

	'LightSeqAttract.Play SeqBlinking, , 5, 150

StopSong

'bonuscheckie

    Dim AwardPoints, TotalBonus, ii
	AwardPoints = 0
    TotalBonus = 0

    'If NOT Tilted Then
	If(Tilted = False) Then
        'Bonus
'		PuPlayer.LabelShowPage pDMD,1,0,""
	
'		PuPlayer.LabelSet pBackglass,"Bonus1", FormatNumber(JackpotsBonus,0),1,"{'mt':2,'color':13264128, 'size': 4 }"
'		AwardPoints = JackpotsBonus * 5000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus2", FormatNumber(RaiderBonus,0),1,"{'mt':2,'color':55660, 'size': 4 }"
'		AwardPoints = RaiderBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus3", FormatNumber(CroftBonus,0),1,"{'mt':2,'color':3739322, 'size': 4 }"
'		AwardPoints = CroftBonus * 2000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus4", FormatNumber(RelicsBonus,0),1,"{'mt':2,'color':25542, 'size': 4 }"
'		AwardPoints = RelicsBonus * 15000
'       TotalBonus = TotalBonus + AwardPoints

'		PuPlayer.LabelSet pBackglass,"Bonus5", FormatNumber(ArtefactsBonus,0),1,"{'mt':2,'color':5602052, 'size': 4 }"
'		AwardPoints = ArtefactsBonus * 10000
        TotalBonus = TotalBonus + AwardPoints
        
'		PuPlayer.LabelSet pBackglass,"BonusTotal", FormatNumber(TotalBonus,0),1,"{'mt':2,'color':16777215, 'size': 4 }"	
        TotalBonus = TotalBonus * BonusMultiplier
       
		SeqBonus
'		vpmtimer.addtimer 2500, "AddScore TotalBonus '"
'		AddScore TotalBonus

		' add a bit of a delay to allow for the bonus points to be shown & added up
		vpmtimer.addtimer 6000, "resetbackglassOFFScore '"
		vpmtimer.addtimer 6000, "EndOfBall2 '"
    Else 'Si hay falta simplemente espera un momento y va directo a la segunta parte después de perder la bola
		BonusDelayTime = 100
		EndOfBall2
    End If
	'vpmtimer.addtimer BonusDelayTime, "EndOfBall2 '"
End Sub

' The Timer which delays the machine to allow any bonus points to be added up
' has expired.  Check to see if there are any extra balls for this player.
' if not, then check to see if this was the last ball (of the CurrentPlayer)
'
Sub EndOfBall2()
    ' if were tilted, reset the internal tilted flag (this will also
    ' set TiltWarnings back to zero) which is useful if we are changing player LOL
    UpdateMusic = UpdateMusic + 1
	UpdateMusicNow	
    Tilted = False
    Tilt = 0
    DisableTable False 'enable again bumpers and slingshots

    ' has the player won an extra-ball ? (might be multiple outstanding)
    If(ExtraBallsAwards(CurrentPlayer) <> 0) Then
        'debug.print "Extra Ball"

        ' yep got to give it to them
        ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) - 1

        ' if no more EB's then turn off any shoot again light
        If(ExtraBallsAwards(CurrentPlayer) = 0) Then
'           LiExtra.State = 0
        End If

        ' You may wish to do a bit of a song AND dance at this point
        'DMD CL(0, "EXTRA BALL"), CL(1, "SHOOT AGAIN"), "", eNone, eNone, eBlink, 1000, True, "vo_extraball"

		UpdateMusic = UpdateMusic - 1
		UpdateMusicNow

        ' reset the playfield for the new ball
        ResetForNewPlayerBall()
		
		' set the dropped wall for bonus

		
        ' Create a new ball in the shooters lane
        CreateNewBall()
    Else ' no extra balls

        BallsRemaining(CurrentPlayer) = BallsRemaining(CurrentPlayer) - 1
		BallinGame(CurrentPlayer) = BallinGame(CurrentPlayer) + 1

        ' was that the last ball ?
        If(BallsRemaining(CurrentPlayer) <= 0) Then
			BallinGame(CurrentPlayer) = BallsPerGame
            'debug.print "No More Balls, High Score Entry"

            ' Submit the CurrentPlayers score to the High Score system
            CheckHighScore()
        ' you may wish to play some music at this point

        Else

            ' not the last ball (for that player)
            ' if multiple players are playing then move onto the next one
            EndOfBallComplete()
        End If
    End If
End Sub

' This function is called when the end of bonus display
' (or high score entry finished) AND it either end the game or
' move onto the next player (or the next ball of the same player)
'
Sub EndOfBallComplete()
    Dim NextPlayer

    'debug.print "EndOfBall - Complete"

    ' are there multiple players playing this game ?
    If(PlayersPlayingGame> 1) Then
        ' then move to the next player
        NextPlayer = CurrentPlayer + 1
        ' are we going from the last player back to the first
        ' (ie say from player 4 back to player 1)
        If(NextPlayer> PlayersPlayingGame) Then
            NextPlayer = 1
        End If
    Else
        NextPlayer = CurrentPlayer
    End If

    'debug.print "Next Player = " & NextPlayer

    ' is it the end of the game ? (all balls been lost for all players)
    If((BallsRemaining(CurrentPlayer) <= 0) AND(BallsRemaining(NextPlayer) <= 0) ) Then
        ' you may wish to do some sort of Point Match free game award here
        ' generally only done when not in free play mode
		StopSong
		'DMD CL(0, "GAME OVER") "", eNone, 13000, True, ""
		'DMD "", CL(1, "GAME OVER"), "", eNone, eNone, eNone, 13000, False, ""
'		PuPEvent 818
        ' set the machine into game over mode
        vpmtimer.addtimer 4000, "EndOfGame() '"
		vpmtimer.addtimer 6800, "pDMDGameOver '"
		
    ' you may wish to put a Game Over message on the desktop/backglass

    Else
        ' set the next player
        CurrentPlayer = NextPlayer

        ' make sure the correct display is up to date
        'DMDScoreNow

        ' reset the playfield for the new player (or new ball)
        ResetForNewPlayerBall()

        ' AND create a new ball
        CreateNewBall()

        ' play a sound if more than 1 player
        If PlayersPlayingGame> 1 Then
            PlaySound "vo_player" &CurrentPlayer
            'DMD "_", CL(1, "PLAYER " &CurrentPlayer), "", eNone, eNone, eNone, 800, True, ""
        End If
    End If
End Sub

' This function is called at the End of the Game, it should reset all
' Drop targets, AND eject any 'held' balls, start any attract sequences etc..

Sub EndOfGame()
    LightSeqAttract.StopPlay
	'debug.print "End Of Game"
    bGameInPLay = False
    ' just ended your game then play the end of game tune
    If NOT bJustStarted Then
        ChangeSong
    End If

    bJustStarted = False
    ' ensure that the flippers are down
    SolLFlipper 0
    SolRFlipper 0
	SolURFlipper 0
	SolULFlipper 0
	

    ' terminate all Mode - eject locked balls
    ' most of the Mode/timers terminate at the end of the ball

    ' set any lights for the attract mode
    GiOff
	B2SGIOff
    StartAttractMode
	ResetEvents
' you may wish to light any Game Over Light you may have
End Sub

Function BallsFunc
    Dim tmp
    tmp = BallsPerGame - BallsRemaining(CurrentPlayer) + 1
    If tmp> BallsPerGame Then
        BallsFunc = BallsPerGame
    Else
        BallsFunc = tmp
    End If
End Function

' *********************************************************************
'                      Drain / Plunger Functions
' *********************************************************************

' lost a ball ;-( check to see how many balls are on the playfield.
' if only one then decrement the remaining count AND test for End of game
' if more than 1 ball (multi-ball) then kill of the ball but don't create
' a new one
'
Sub Drain_Hit()
    ' Destroy the ball
    Drain.DestroyBall
    BallsOnPlayfield = BallsOnPlayfield - 1 
	'If BallsOnPlayfield<2 Then
	'Multiball=false
	'end if
	
    ' pretend to knock the ball into the ball storage mech
 '   PlaySoundAt "fx_drain", Drain 'burger
	RandomSoundDrain Drain 'burger
    'if Tilted then end Ball Mode
    If Tilted Then
        StopEndOfBallMode
    End If

    ' if there is a game in progress AND it is not Tilted
    If(bGameInPLay = True) AND(Tilted = False) Then

        ' is the ball saver active,
        If(bBallSaverActive = True) Then
	AddMultiball 1
	Playsound "Fx_SaveBall"
	DOF 316, DOFPulse
	PlaySoundAt SoundFXDOF("", 120, DOFPulse, DOFContactors), BallRelease 'burger Nom du son supprimé entre guillemets
	RandomSoundBallRelease Ballrelease 'burger
	pDMDmessage1L "Ball Saved",1
	pDMDLabelFadePulse "Texte1a", 2500, clGold
	if UseUltraDMD = 1 then UDMD "BALL SAVED", "", 3000
	bAutoPlunger = True
            ' yep, create a new ball in the shooters lane
            ' we use the Addmultiball in case the multiballs are being ejected
    'vpmtimer.addtimer 1250, "CreateNewBall() '"

           ' you may wish to put something on a display or play a sound at this point

            
        Else

			If(BallsOnPlayfield = 1)Then
                ' AND in a multi-ball??
                If(bMultiBallMode = True)then
                    ' not in multiball mode any more
					Mode_Multiball_End
                    bMultiBallMode = False		
                    ' you may wish to change any music over at this point and
                    ' turn off any multiball specific lights
					StopSong
'					ChangeSong
                End If
            End If
            ' was that the last ball on the playfield
            If(BallsOnPlayfield = 0) Then
                   if halplay=1 Then 'halplay
                   kicker025.enabled=1'halplay
                   end If'halplay
                ' End Mode and timers
				StopSong
				StopGenieFree
				PlaySoundEND
                StopEndOfBallMode
                vpmtimer.addtimer 200, "EndOfBall '" 'the delay is depending of the animation of the end of ball, since there is no animation then move to the end of ball
            End If
        End If
    End If
End Sub



' The Ball has rolled out of the Plunger Lane and it is pressing down the trigger in the shooters lane
' Check to see if a ball saver mechanism is needed and if so fire it up.

Sub Trigger1_Hit()
If bAutoPlunger Then
        'debug.print "autofire the ball"
        PlungerIM.AutoFire
        DOF 120, DOFPulse
		DOF 316, DOFPulse
        PlaySoundAt "fx_fire", Trigger1
        bAutoPlunger = False
    End If	
'StopSong

    bBallInPlungerLane = True
	DOF 317, DOFOn 'DOF MX - Ball Ready to Launch - ON
	broomdone = 0
	GiOn
    If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then
'        EnableBallSaver BallSaverTime
'        Else
'        ' show the message to shoot the ball in case the player has fallen sleep
'        Trigger1.TimerEnabled = 1
    End If
End Sub

Sub Gate001_Hit()
	AddScore 2550
	DOF 317, DOFoff
	ChangeGILogoNow
'	CheckSkillShot
	If(bBallSaverReady = True) AND(BallSaverTime <> 0) And(bBallSaverActive = False) Then 
        EnableBallSaver BallSaverTime
'		ResetSkillShotTimer.Enabled = True
        Else
        ' show the message to shoot the ball in case the player has fallen sleep
        Trigger1.TimerEnabled = 1
    End If
End Sub
' The ball is released from the plunger

Sub Trigger1_UnHit()
    bBallInPlungerLane = False
	DOF 317, DOFOff 'DOF MX - Ball Ready to Launch - Off
    'LightEffect 4
	'ChangeSong
End Sub


Sub Trigger1_Timer
    trigger1.TimerEnabled = 0
End Sub

Sub EnableBallSaver(seconds)
    'debug.print "Ballsaver started"
    ' set our game flag
    bBallSaverActive = True
    bBallSaverReady = False
    ' start the timer
    BallSaverTimer.Interval = 1000 * seconds
    BallSaverTimer.Enabled = True
    BallSaverSpeedUpTimer.Interval = 1000 * seconds -(1000 * seconds) / 3
    BallSaverSpeedUpTimer.Enabled = True
    ' if you have a ball saver light you might want to turn it on at this point (or make it flash)
    LightShootAgain.BlinkInterval = 160
    LightShootAgain.State = 2
End Sub

' The ball saver timer has expired.  Turn it off AND reset the game flag
'
Sub BallSaverTimer_Timer()
    'debug.print "Ballsaver ended"
    BallSaverTimer.Enabled = False
    ' clear the flag
    bBallSaverActive = False
    ' if you have a ball saver light then turn it off at this point
   LightShootAgain.State = 0
End Sub
Sub BallSaverEnd
	BallSaverTimer.Enabled = False
    bBallSaverActive = False
    LightShootAgain.State = 0
End Sub

Sub BallSaverSpeedUpTimer_Timer()
    'debug.print "Ballsaver Speed Up Light"
    BallSaverSpeedUpTimer.Enabled = False
    ' Speed up the blinking
    LightShootAgain.BlinkInterval = 80
    LightShootAgain.State = 2
End Sub

' *********************************************************************
'                      Supporting Score Functions
' *********************************************************************

' Add points to the score AND update the score board
Sub AddScore(points)
    If Tilted Then Exit Sub

    ' add the points to the current players score variable
 '   PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + points
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + (points * PlayfieldMultiplier)

    ' you may wish to check to see if the player has gotten an extra ball by a high score
    If PlayerScore(CurrentPlayer) >= Special1 AND Special1Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special1Awarded(CurrentPlayer) = True
    End If
    If PlayerScore(CurrentPlayer) >= Special2 AND Special2Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special2Awarded(CurrentPlayer) = True
    End If
    If PlayerScore(CurrentPlayer) >= Special3 AND Special3Awarded(CurrentPlayer) = False Then
        AwardExtraBall
        Special3Awarded(CurrentPlayer) = True
    End If
End Sub

Sub AddScore2(points)
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + points
End Sub
' Add bonus to the bonuspoints AND update the score board
Sub AddBonus(points) 'not used in this table, since there are many different bonus items.
    If(Tilted = False) Then
        ' add the bonus to the current players bonus variable
        BonusPoints(CurrentPlayer) = BonusPoints(CurrentPlayer) + points
    End if
End Sub

Sub AwardExtraBall()
    ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	pDMDmessage1L "Extraball",1
	pDMDLabelFadePulse "Texte1a", 2500, clGreen
	if UseUltraDMD = 1 then UDMD "EXTRABALL", "", 3000
	JasmineCount(CurrentPlayer) = 7
	UpDateBGimage
	PauseDMDHarem = 1
	PauseDMDHaremTimer.Enabled=True
	UpdateDMDHarem
	DOF 121, DOFPulse
	DOF 451, DOFPulse
'	PuPEvent 835
'	LiExtra.state = 1 
'    LightShootAgain.State = 1
    LightEffect 2
End Sub


'********************************************************************************************
' Only for VPX 10.2 and higher.
' FlashForMs will blink light or a flasher for TotalPeriod(ms) at rate of BlinkPeriod(ms)
' When TotalPeriod done, light or flasher will be set to FinalState value where
' Final State values are:   0=Off, 1=On, 2=Return to previous State
'********************************************************************************************

Sub FlashForMs(MyLight, TotalPeriod, BlinkPeriod, FinalState) 'thanks gtxjoe for the first version

    If TypeName(MyLight) = "Light" Then

        If FinalState = 2 Then
            FinalState = MyLight.State 'Keep the current light state
        End If
        MyLight.BlinkInterval = BlinkPeriod
        MyLight.Duration 2, TotalPeriod, FinalState
    ElseIf TypeName(MyLight) = "Flasher" Then

        Dim steps

        ' Store all blink information
        steps = Int(TotalPeriod / BlinkPeriod + .5) 'Number of ON/OFF steps to perform
        If FinalState = 2 Then                      'Keep the current flasher state
            FinalState = ABS(MyLight.Visible)
        End If
        MyLight.UserValue = steps * 10 + FinalState 'Store # of blinks, and final state

        ' Start blink timer and create timer subroutine
        MyLight.TimerInterval = BlinkPeriod
        MyLight.TimerEnabled = 0
        MyLight.TimerEnabled = 1
        ExecuteGlobal "Sub " & MyLight.Name & "_Timer:" & "Dim tmp, steps, fstate:tmp=me.UserValue:fstate = tmp MOD 10:steps= tmp\10 -1:Me.Visible = steps MOD 2:me.UserValue = steps *10 + fstate:If Steps = 0 then Me.Visible = fstate:Me.TimerEnabled=0:End if:End Sub"
    End If
End Sub


' ********************************
'   Table info & Attract Mode
' ********************************

Sub StartLightSeq()
    'lights sequences
    LightSeqAttract.UpdateInterval = 25
    LightSeqAttract.Play SeqBlinking, , 5, 150
    LightSeqAttract.Play SeqRandom, 40, , 4000
    LightSeqAttract.Play SeqAllOff
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 50, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 40, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 40, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqRightOn, 30, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqLeftOn, 30, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 10
    LightSeqAttract.Play SeqCircleOutOn, 15, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 5
    LightSeqAttract.Play SeqStripe1VertOn, 50, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqCircleOutOn, 15, 2
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 50, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 25, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe1VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqStripe2VertOn, 25, 3
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqUpOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqDownOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqRightOn, 15, 1
    LightSeqAttract.UpdateInterval = 8
    LightSeqAttract.Play SeqLeftOn, 15, 1
End Sub

Sub LightSeqAttract_PlayDone()
    StartLightSeq()
End Sub

Sub LightSeqTilt_PlayDone()
    LightSeqTilt.Play SeqAllOff
End Sub

'***********************************************************************
' *********************************************************************
'                     Table Specific Script Starts Here
' *********************************************************************
'***********************************************************************

' droptargets, animations, etc
Sub VPObjects_Init
End Sub

' tables variables and Mode init
'Dim HoleBonus, BumperBonus, ALLRampBonus, RampBonus1, RampBonus2, RampBonus3, MulitballBonus, TargetBonus    
Dim JackpotsBonus, RaiderBonus, CroftBonus, RelicsBonus, ArtefactsBonus

Sub Game_Init() 'called at the start of a new game
	playclear pBackglass
    Dim i, j
    ChangeSong
	ResetEvents
'	TargetBonus = 0
	'bumperHits = 100
'	BumperBonus = 0
'	MulitballBonus = 0
	'BallInHole = 0
    TurnOffPlayfieldLights()
End Sub

Sub StopEndOfBallMode()     'this sub is called after the last ball is drained
End Sub

Sub ResetNewBallVariables() 'reset variables for a new ball or player
Dim i
TargetBonus = 0
bBallSaverReady = True
End Sub

Sub ResetNewBallLights()    'turn on or off the needed lights before a new ball is released
	gi1.state = 1
	gi2.state = 1
	gi3.state = 1
	gi4.state = 1
End Sub

Sub TurnOffPlayfieldLights()
    Dim a
    For each a in aLights
        a.State = 0
    Next
End Sub

' *********************************************************************
'                        Table Object Hit Events
'
' Any target hit Sub should do something like this:
' - play a sound
' - do some physical movement
' - add a score, bonus
' - check some variables/Mode this trigger is a member of
' - set the "LastSwitchHit" variable in case it is needed later
' *********************************************************************


'****************************************************************
'	ZSLG: Slingshots
'****************************************************************

' RStep and LStep are the variables that increment the animation
Dim RStep, LStep, LStep2

Sub RightSlingShot_Slingshot
	PlaySoundAt SoundFXDOF("Sling_L", 104, DOFPulse, DOFcontactors), Sling1
    DOF 111, DOFPulse
	RS.VelocityCorrect(ActiveBall)
	Addscore 510
'	If (RaishaActive = 1 or NajwaActive = 1 or TasmineActive = 1) And PauseDMDHarem = 0 And PauseDMDHaremTimer.Enabled = False Then UpdateDMDHarem 'Debug
	If (RaishaActive = 1 or NajwaActive = 1 or TasmineActive = 1) And PauseDMDHarem = 0 Then PauseDMDHaremTimer.Enabled = False : UpdateDMDHarem 'Debug
	SlingCount = SlingCount + 1
	FlashForMs F1A004, 500, 250, 0
	RSling1.Visible = 1
	Sling1.TransY =  - 20   'Sling Metal Bracket
	RStep = 0
	RightSlingShot.TimerEnabled = 1
	RightSlingShot.TimerInterval = 10
	'   vpmTimer.PulseSw 52	'Slingshot Rom Switch
	RandomSoundSlingshotRight Sling1
	If AppleCount(CurrentPlayer) <= 5 Then Apple002.visible = 1 : Apple003.visible = 1
	If SultanActive = 0 And PieceCount(CurrentPlayer) <= 5 Then AladpieceRottimer.enabled=True
	DebugHaremDMD
End Sub

Sub RightSlingShot_Timer
	Select Case RStep
		Case 3
			RSLing1.Visible = 0
			RSLing2.Visible = 1
			Sling1.TransY =  - 10
		Case 4
			RSLing2.Visible = 0
			Sling1.TransY = 0
			RightSlingShot.TimerEnabled = 0
	End Select
	RStep = RStep + 1
End Sub

Sub LeftSlingShot_Slingshot
	PlaySoundAt SoundFXDOF("Sling_R", 103, DOFPulse, DOFcontactors), Sling2
    DOF 110, DOFPulse
	LS.VelocityCorrect(ActiveBall)
	Addscore 510
'	If (RaishaActive = 1 or NajwaActive = 1 or TasmineActive = 1) And PauseDMDHarem = 0 And PauseDMDHaremTimer.Enabled = False Then UpdateDMDHarem 'Debug
	If (RaishaActive + NajwaActive + TasmineActive) >= 1 And PauseDMDHarem = 0 Then PauseDMDHaremTimer.Enabled = False : UpdateDMDHarem 'Debug
	SlingCount = SlingCount + 1
'	FlashForMs F1A001, 1000, 50, 0
	FlashForMs F1A001, 500, 250, 0
	LSling1.Visible = 1
	Sling2.TransY =  - 20   'Sling Metal Bracket
	LStep = 0
	LeftSlingShot.TimerEnabled = 1
	LeftSlingShot.TimerInterval = 10
	'   vpmTimer.PulseSw 51	'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling2
	If AppleCount(CurrentPlayer) <= 5 Then Apple002.visible = 1 : Apple003.visible = 1
	If SultanActive = 0 And PieceCount(CurrentPlayer) <= 5 Then AladpieceRottimer.enabled=True
	DebugHaremDMD
End Sub

Sub LeftSlingShot_Timer
	Select Case LStep
		Case 3
			LSLing1.Visible = 0
			LSLing2.Visible = 1
			Sling2.TransY =  - 10
		Case 4
			LSLing2.Visible = 0
			Sling2.TransY = 0
			LeftSlingShot.TimerEnabled = 0
	End Select
	LStep = LStep + 1
End Sub

Sub LeftSlingShot2_Slingshot
	LS.VelocityCorrect(ActiveBall)
	Addscore 510
	PlaySoundAt SoundFXDOF("Sling_R", 103, DOFPulse, DOFcontactors), Sling2
	DOF 110, DOFPulse
	SlingCount = SlingCount + 1
	FlashForMs F1A009, 500, 250, 0
	LSling002.Visible = 1
	Sling001.TransY =  - 20   'Sling Metal Bracket
	LStep2 = 0
	LeftSlingShot2.TimerEnabled = 1
	LeftSlingShot2.TimerInterval = 10
	'   vpmTimer.PulseSw 51	'Slingshot Rom Switch
	RandomSoundSlingshotLeft Sling001
End Sub

Sub LeftSlingShot2_Timer
	Select Case LStep2
		Case 3
			LSLing002.Visible = 0
			LSLing001.Visible = 1
			Sling001.TransY =  - 10
		Case 4
			LSLing001.Visible = 0
			Sling001.TransY = 0
			LeftSlingShot2.TimerEnabled = 0
	End Select
	LStep2 = LStep2 + 1
End Sub

Sub TestSlingShot_Slingshot
	TS.VelocityCorrect(ActiveBall)
End Sub

'**************************
'Flupper bumpers
'**************************

Dim DayNightAdjust , DNA30, DNA45, DNA90
If NightDay < 10 Then
	DNA30 = 0 : DNA45 = (NightDay-10)/20 : DNA90 = 0 : DayNightAdjust = 0.4
Else
	DNA30 = (NightDay-10)/30 : DNA45 = (NightDay-10)/45 : DNA90 = (NightDay-10)/90 : DayNightAdjust = NightDay/25
End If

Dim FlBumperFadeActual(6), FlBumperFadeTarget(6), FlBumperColor(6), FlBumperTop(6), FlBumperSmallLight(6), Flbumperbiglight(6)
Dim FlBumperDisk(6), FlBumperBase(6), FlBumperBulb(6), FlBumperscrews(6), FlBumperActive(6), FlBumperHighlight(6)
Dim cnt : For cnt = 1 to 6 : FlBumperActive(cnt) = False : Next

' colors available are red, white, blue, orange, yellow, green, purple and blacklight

FlInitBumper 1, "red"
FlInitBumper 2, "red"
FlInitBumper 3, "red"

' ### uncomment the statement below to change the color for all bumpers ###
' Dim ind : For ind = 1 to 5 : FlInitBumper ind, "green" : next

Sub FlInitBumper(nr, col)
	FlBumperActive(nr) = True
	' store all objects in an array for use in FlFadeBumper subroutine
	FlBumperFadeActual(nr) = 1 : FlBumperFadeTarget(nr) = 1.1: FlBumperColor(nr) = col
	Set FlBumperTop(nr) = Eval("bumpertop" & nr) : FlBumperTop(nr).material = "bumpertopmat" & nr
	Set FlBumperSmallLight(nr) = Eval("bumpersmalllight" & nr) : Set Flbumperbiglight(nr) = Eval("bumperbiglight" & nr)
	Set FlBumperDisk(nr) = Eval("bumperdisk" & nr) : Set FlBumperBase(nr) = Eval("bumperbase" & nr)
	Set FlBumperBulb(nr) = Eval("bumperbulb" & nr) : FlBumperBulb(nr).material = "bumperbulbmat" & nr
	Set FlBumperscrews(nr) = Eval("bumperscrews" & nr): FlBumperscrews(nr).material = "bumperscrew" & col
	Set FlBumperHighlight(nr) = Eval("bumperhighlight" & nr)
	' set the color for the two VPX lights
	select case col
		Case "red"
			FlBumperSmallLight(nr).color = RGB(255,4,0) : FlBumperSmallLight(nr).colorfull = RGB(255,24,0)
			FlBumperBigLight(nr).color = RGB(255,32,0) : FlBumperBigLight(nr).colorfull = RGB(255,32,0)
			FlBumperHighlight(nr).color = RGB(64,255,0)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.98
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "blue"
			FlBumperBigLight(nr).color = RGB(32,80,255) : FlBumperBigLight(nr).colorfull = RGB(32,80,255)
			FlBumperSmallLight(nr).color = RGB(0,80,255) : FlBumperSmallLight(nr).colorfull = RGB(0,80,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : MaterialColor "bumpertopmat" & nr, RGB(8,120,255)
			FlBumperHighlight(nr).color = RGB(255,16,8)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "green"
			FlBumperSmallLight(nr).color = RGB(8,255,8) : FlBumperSmallLight(nr).colorfull = RGB(8,255,8)
			FlBumperBigLight(nr).color = RGB(32,255,32) : FlBumperBigLight(nr).colorfull = RGB(32,255,32)
			FlBumperHighlight(nr).color = RGB(255,32,255) : MaterialColor "bumpertopmat" & nr, RGB(16,255,16) 
			FlBumperSmallLight(nr).TransmissionScale = 0.005
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "orange"
			FlBumperHighlight(nr).color = RGB(255,130,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).color = RGB(255,130,0) : FlBumperSmallLight(nr).colorfull = RGB (255,90,0)
			FlBumperBigLight(nr).color = RGB(255,190,8) : FlBumperBigLight(nr).colorfull = RGB(255,190,8)
		Case "white"
			FlBumperBigLight(nr).color = RGB(255,230,190) : FlBumperBigLight(nr).colorfull = RGB(255,230,190)
			FlBumperHighlight(nr).color = RGB(255,180,100) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 0.99
		Case "blacklight"
			FlBumperBigLight(nr).color = RGB(32,32,255) : FlBumperBigLight(nr).colorfull = RGB(32,32,255)
			FlBumperHighlight(nr).color = RGB(48,8,255) : 
			FlBumperSmallLight(nr).TransmissionScale = 0
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
		Case "yellow"
			FlBumperSmallLight(nr).color = RGB(255,230,4) : FlBumperSmallLight(nr).colorfull = RGB(255,230,4)
			FlBumperBigLight(nr).color = RGB(255,240,50) : FlBumperBigLight(nr).colorfull = RGB(255,240,50)
			FlBumperHighlight(nr).color = RGB(255,255,220)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1 
			FlBumperSmallLight(nr).TransmissionScale = 0
		Case "purple"
			FlBumperBigLight(nr).color = RGB(80,32,255) : FlBumperBigLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).color = RGB(80,32,255) : FlBumperSmallLight(nr).colorfull = RGB(80,32,255)
			FlBumperSmallLight(nr).TransmissionScale = 0 : 
			FlBumperHighlight(nr).color = RGB(32,64,255)
			FlBumperSmallLight(nr).BulbModulateVsAdd = 1
	end select
End Sub

Sub FlFadeBumper(nr, Z)
	FlBumperBase(nr).BlendDisableLighting = 0.5 * DayNightAdjust
'	UpdateMaterial(string, float wrapLighting, float roughness, float glossyImageLerp, float thickness, float edge, float edgeAlpha, float opacity,
'               OLE_COLOR base, OLE_COLOR glossy, OLE_COLOR clearcoat, VARIANT_BOOL isMetal, VARIANT_BOOL opacityActive,
'               float elasticity, float elasticityFalloff, float friction, float scatterAngle) - updates all parameters of a material
	FlBumperDisk(nr).BlendDisableLighting = (0.5 - Z * 0.3 )* DayNightAdjust	

	select case FlBumperColor(nr)

		Case "blue" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(38-24*Z,130 - 98*Z,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20  + 500 * Z / (0.5 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 5000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 45 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 10000 * (Z^3) / (0.5 + DNA90)

		Case "green"	
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(16 + 16 * sin(Z*3.14),255,16 + 16 * sin(Z*3.14)), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 10 + 150 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 2 * DayNightAdjust + 20 * Z
			FlBumperBulb(nr).BlendDisableLighting = 7 * DayNightAdjust + 6000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 6000 * (Z^3) / (1 + DNA90)
		
		Case "red" 
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 16 - 11*Z + 16 * sin(Z*3.14),0), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 100 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 18 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 20 * DayNightAdjust + 9000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,20 + Z*4,8-Z*8)
		
		Case "orange"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 100 - 22*z  + 16 * sin(Z*3.14),Z*32), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 250 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 2500 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,100 + Z*50, 0)

		Case "white"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255,230 - 100 * Z, 200 - 150 * Z), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 180 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 5 * DayNightAdjust + 30 * Z
			FlBumperBulb(nr).BlendDisableLighting = 18 * DayNightAdjust + 3000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 14 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255,255 - 20*Z,255-65*Z) : FlBumperSmallLight(nr).colorfull = RGB(255,255 - 20*Z,255-65*Z)
			MaterialColor "bumpertopmat" & nr, RGB(255,235 - z*36,220 - Z*90)

		Case "blacklight"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 1, RGB(30-27*Z^0.03,30-28*Z^0.01, 255), RGB(255,255,255), RGB(32,32,32), false, true, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 20 + 900 * Z / (1 + DNA30)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 60 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 30000 * Z^3
			Flbumperbiglight(nr).intensity = 40 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 2000 * (Z^3) / (1 + DNA90)
			FlBumperSmallLight(nr).color = RGB(255-240*(Z^0.1),255 - 240*(Z^0.1),255) : FlBumperSmallLight(nr).colorfull = RGB(255-200*z,255 - 200*Z,255)
			MaterialColor "bumpertopmat" & nr, RGB(255-190*Z,235 - z*180,220 + 35*Z)

		Case "yellow"
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(255, 180 + 40*z, 48* Z), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 17 + 200 * Z / (1 + DNA30^2)
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 40 * Z / (1 + DNA90)
			FlBumperBulb(nr).BlendDisableLighting = 12 * DayNightAdjust + 2000 * (0.03 * Z +0.97 * Z^10)
			Flbumperbiglight(nr).intensity = 20 * Z / (1 + DNA45)
			FlBumperHighlight(nr).opacity = 1000 * (Z^3) / (1 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(255,200, 24 - 24 * z)

		Case "purple" :
			UpdateMaterial "bumperbulbmat" & nr, 0, 0.75 , 0, 1-Z, 1-Z, 1-Z, 0.9999, RGB(128-118*Z - 32 * sin(Z*3.14), 32-26*Z ,255), RGB(255,255,255), RGB(32,32,32), false, True, 0, 0, 0, 0
			FlBumperSmallLight(nr).intensity = 15  + 200 * Z / (0.5 + DNA30) 
			FlBumperTop(nr).BlendDisableLighting = 3 * DayNightAdjust + 50 * Z
			FlBumperBulb(nr).BlendDisableLighting = 15 * DayNightAdjust + 10000 * (0.03 * Z +0.97 * Z^3)
			Flbumperbiglight(nr).intensity = 50 * Z / (1 + DNA45) 
			FlBumperHighlight(nr).opacity = 4000 * (Z^3) / (0.5 + DNA90)
			MaterialColor "bumpertopmat" & nr, RGB(128-60*Z,32,255)


	end select
End Sub

Sub BumperTimer_Timer
	dim nr
	For nr = 1 to 6
		If FlBumperFadeActual(nr) < FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.8
			If FlBumperFadeActual(nr) > 0.99 Then FlBumperFadeActual(nr) = 1 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
		If FlBumperFadeActual(nr) > FlBumperFadeTarget(nr) and FlBumperActive(nr)  Then
			FlBumperFadeActual(nr) = FlBumperFadeActual(nr) + (FlBumperFadeTarget(nr) - FlBumperFadeActual(nr)) * 0.4 / (FlBumperFadeActual(nr) + 0.1)
			If FlBumperFadeActual(nr) < 0.01 Then FlBumperFadeActual(nr) = 0 : End If
			FlFadeBumper nr, FlBumperFadeActual(nr)
		End If
	next
End Sub

'************************** 
'Bumpers 
'************************** 
Dim bumperHits

Sub Bumper1_Hit()
	bumperHits = bumperHits - 1
	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("", 109, DOFPulse, DOFContactors), Bumper1  'burger
		RandomSoundBumperTop Bumper1 'burger
       DOF 138, DOFPulse
		pDMDLabelPulseText2 "Bumper001","Hip",500,clWhite
		GiEffect 4
		AddScore 1815
		If ModeSablier = True Then AddScore2 7260
		FlBumperFadeTarget(1) = 0
		bumper1.timerinterval = 100
		Bumper1.timerenabled = True
		If NajwaActive = 1 Then
			NajwaCount = NajwaCount - 1
	'		DispXText BumperCount
			If NajwaCount = 0 Then
			AdwardNajwa
			end If
		end If
	End If
End Sub

Sub Bumper2_Hit()
	bumperHits = bumperHits - 1
	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("", 107, DOFPulse, DOFContactors), Bumper2 'burger
		RandomSoundBumperMiddle Bumper2
       DOF 139, DOFPulse
		pDMDLabelPulseText2 "Bumper002","Hop",500,clWhite
		GiEffect 4
		AddScore 1815
		If ModeSablier = True Then AddScore2 7260
		FlBumperFadeTarget(2) = 0
		bumper2.timerinterval = 100
		Bumper2.timerenabled = True
		If NajwaActive = 1 Then
			NajwaCount = NajwaCount - 1
	'		DispXText BumperCount
			If NajwaCount = 0 Then
			AdwardNajwa
			end If
		end If
	End If
End Sub

Sub Bumper3_Hit()
	bumperHits = bumperHits - 1
	CheckBumpers
	If NOT Tilted Then
        PlaySoundAt SoundFXDOF("", 108, DOFPulse, DOFContactors), Bumper3 'burger
		RandomSoundBumperBottom Bumper3
       DOF 140, DOFPulse
		pDMDLabelPulseText2 "Bumper003","Bam",500,clWhite
		GiEffect 4
		AddScore 1815
		If ModeSablier = True Then AddScore2 7260
		FlBumperFadeTarget(3) = 0
		bumper3.timerinterval = 100
		Bumper3.timerenabled = True
		If NajwaActive = 1 Then
			NajwaCount = NajwaCount - 1
	'		DispXText BumperCount
			If NajwaCount = 0 Then
			AdwardNajwa
			end If
		end If
	End If
End Sub

Sub Bumper1_Timer()
	FlBumperFadeTarget(1) = 1
End Sub

Sub Bumper2_Timer()
	FlBumperFadeTarget(2) = 1
End Sub

Sub Bumper3_Timer()
	FlBumperFadeTarget(3) = 1
End Sub

' Bumper Bonus
' 100000 i bonus after each 100 hits

Sub CheckBumpers()
	If bumperHits <= 0 Then
	BumperCount = BumperCount + 1
	PauseDMDHarem = 1
	PauseDMDHaremTimer.Enabled = True
	UpdateDMDHarem
	Pupevent 848
	ResetBumpers
	End If
End Sub

Sub ResetBumpers
    bumperHits = 15
End Sub


'' ###################################
'' ###### copy script until here #####
'' ###################################

'*****************
'Gates
'*****************
Sub Gate008_Hit() 'enter PF UP
    If ActiveBall.VelY > 0 Then
        WireRampOff
    Else
        WireRampOn True   ' going up → start
    End If

	AddScore 75
	DOF 443, DOFPulse
End Sub

Sub Trigger012_Hit()
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	FlashForMs F1A007, 500, 250, 0
End Sub
Sub Trigger013_Hit()
	AddScore 750
	CheckJump
	FlashForMs F1A006, 500, 250, 0
	FlashBotelR.Visible = 0
	F1A005.state = 0
End Sub
'*****************
'Kickers
'*****************

sub kicker001_hit()
	AddScore 355
	JafaEyes.Visible = 1
	JafaCannEyes.Visible = 1
	If StoryCount(CurrentPlayer) >= 1 And StoryCount(CurrentPlayer) < 11 And JafaScoopCount < 3 Then JafaScoopCount = JafaScoopCount + 1
	If bMultiBallMode = False And (StoryCount(CurrentPlayer) = 2 or StoryCount(CurrentPlayer) = 4 or StoryCount(CurrentPlayer) = 8) Then
		Playsound "fx_hole_enter"
		kicker001.destroyball
	PauseDMDHarem = 1
		UpdateDMDHarem
		StartMegaTapi
	Elseif bMultiBallMode = False And StoryCount(CurrentPlayer) = 6 Then
		Playsound "fx_hole_enter"
		kicker001.destroyball
	ResetGirl
		StartBoss
	Elseif bMultiBallMode = False And StoryCount(CurrentPlayer) = 10 Then
		Playsound "fx_hole_enter"
		kicker001.destroyball
	ResetGirl
		StartModeSnake
	Elseif bMultiBallMode = False And JafaScoopCount = 2 And (StoryCount(CurrentPlayer) = 1 or StoryCount(CurrentPlayer) = 3 or StoryCount(CurrentPlayer) = 5 or StoryCount(CurrentPlayer) = 7 or StoryCount(CurrentPlayer) = 9) Then
		LancerRoueAladdin
		soundsaucerlock 'burger
		Kicker001.timerinterval = 9000
		kicker001.timerenabled = True
	Else
		If JafaScoopCount = 1 Then PauseDMDHarem = 1 : UpdateDMDHarem : PuPEvent 839
		soundsaucerlock 'burger
		Kicker001.timerinterval = 2000
		kicker001.timerenabled = True
	End If
end Sub
Sub Kicker001_Timer()
	CheckAwardJafa
	JafaEyes.Visible = 0
	JafaCannEyes.Visible = 0
	Playsound SoundFXDOF("fx_kicker" , 112, DOFPulse, DOFContactors)'burger
	SoundSaucerKick 1, Kicker001 'burger
	kicker001.kick 179, 10
	kicker001.timerenabled = False
End Sub

Sub CheckAwardJafa
	If JafaScoopCount >= 3 And (StoryCount(CurrentPlayer) = 1 or StoryCount(CurrentPlayer) = 3 or StoryCount(CurrentPlayer) = 5 or StoryCount(CurrentPlayer) = 7 or StoryCount(CurrentPlayer) = 9) Then
	StoryCount(CurrentPlayer) = StoryCount(CurrentPlayer) + 1
	CheckStoryCount
	AddScore 30000
	End If
End Sub

sub kicker002_hit()
	AddScore 355
'	Playsound "fx_kicker_enter" 'burger
	soundsaucerlock 'burger
	FlashForMs F1A009, 500, 100, 0
	If LampSoutCollected(CurrentPlayer) < 2 Then JafaCannTimer.enabled=True
	Kicker002.timerinterval = 3000
	kicker002.timerenabled = True
end Sub

Sub Kicker002_Timer()
	If LampSoutCollected(CurrentPlayer) < 2 Then
		ResetGirl
		StartModeSouterrain
		If LampSouterrain(CurrentPlayer) = 0 Then PupEvent 841
		If LampSouterrain(CurrentPlayer) >= 1 Then PupEvent 846
		If usePuPDMD = False Then Playsound "Vo_StartCav"
'		kicker002.kick 161, 10
		kicker002.timerenabled = False
	Elseif LampSoutCollected(CurrentPlayer) >= 2 Then 
		If Light008.State = 2 Then 
			Light008.State = 0 
			ResetCoeurCount
			JasmineCount(CurrentPlayer) = JasmineCount(CurrentPlayer) + 1
			pDMDLabelPulseImage2 "CoeurAnim",2000,0
			pDMDLabelPulseText2 "CoeurAdd","+1",2000,clWhite
			PauseDMDHarem = 1 
			PauseDMDHaremTimer.Enabled = True
			UpdateDMDHarem 
			SelectAwardJasmine
			UpDateBGimage
		Elseif Light008.State = 0 Then
			If JasmineCount(CurrentPlayer) <= 5 Then
			CoeurCount(CurrentPlayer) = CoeurCount(CurrentPlayer) + 1
			CheckCoeurCount
			End If
		End If 
		If (TasmineActive + RaishaActive + NajwaActive) = 0 Then
			Kicker005.CreateSizedBallWithMass BallSize / 2, BallMass
			Kicker005.timerinterval = 2000
			kicker005.timerenabled = True
			Playsound "fx_electro"
			FlashForMs F1A010, 1000, 100, 0
			DOF 449, DOFPulse 'mx left Electro
			kicker002.destroyball
			kicker002.timerenabled = False	
		Elseif (TasmineActive + RaishaActive + NajwaActive) >= 1 Then
			'PlaySound "fx_kicker" 'burger
			playsoundat "warehousekick", Kicker002 'burger
			kicker002.kick 161, 10
			kicker002.timerenabled = False
		End If
	End If 
End Sub

Sub SelectAwardJasmine
	If JasmineCount(CurrentPlayer) = 1 Then AddScore2 25000 : pDMDmessage2L "Jasmine Feeling","+ 25 K",1 : If usePuPDMD = False Then PlaySound "Vo_Jasmine001" Else PuPEvent 871 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 25K", 3000
	If JasmineCount(CurrentPlayer) = 2 Then AddScore2 50000 : pDMDmessage2L "Jasmine Feeling","+ 50 K",1 : If usePuPDMD = False Then PlaySound "Vo_Jasmine002" Else PuPEvent 872 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 50K", 3000
	If JasmineCount(CurrentPlayer) = 3 Then AddScore2 150000 : pDMDmessage2L "Jasmine Feeling","+ 150 K",1 : If usePuPDMD = False Then PlaySound "Vo_Jasmine003" Else PuPEvent 873 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 150K", 3000
	If JasmineCount(CurrentPlayer) = 4 Then AddScore2 500000 : pDMDmessage2L "Jasmine Feeling","+ 500 K",1 : If usePuPDMD = False Then PlaySound "Vo_Jasmine004" Else PuPEvent 874 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 500K", 3000
	If JasmineCount(CurrentPlayer) = 5 Then AddScore2 1000000 : pDMDmessage2L "Jasmine Feeling","Award 1 M",1 : If usePuPDMD = False Then PlaySound "Vo_Jasmine0054" Else PuPEvent 875 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 1M", 3000
	If JasmineCount(CurrentPlayer) = 6 Then JasmineCount(CurrentPlayer) = 7 : AwardExtraBall
End Sub

Sub SelectAwardJasmineALT
	If JasmineCount(CurrentPlayer) = 1 Then AddScore2 25000 : pDMDmessage2L "Jasmine Feeling","+ 25 K",1 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 25K", 3000
	If JasmineCount(CurrentPlayer) = 2 Then AddScore2 50000 : pDMDmessage2L "Jasmine Feeling","+ 50 K",1 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 50K", 3000
	If JasmineCount(CurrentPlayer) = 3 Then AddScore2 150000 : pDMDmessage2L "Jasmine Feeling","+ 150 K",1 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 150K", 3000
	If JasmineCount(CurrentPlayer) = 4 Then AddScore2 500000 : pDMDmessage2L "Jasmine Feeling","+ 500 K",1 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 500K", 3000
	If JasmineCount(CurrentPlayer) = 5 Then AddScore2 1000000 : pDMDmessage2L "Jasmine Feeling","Award 1 M",1 : if UseUltraDMD = 1 then UDMD "JASMINE UP", "BONUS 1M", 3000
	If JasmineCount(CurrentPlayer) = 6 Then JasmineCount(CurrentPlayer) = 7 : AwardExtraBall
End Sub

sub kicker004_hit() 'LockMB
	AddScore 1505
	Playsound "fx_hole_enter"
	If MBLock_Sav(CurrentPlayer) = 1 or MBLock_Sav(CurrentPlayer) = 3 or MBLock_Sav(CurrentPlayer) = 5 Then
		MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1
			If MBLock_Sav(CurrentPlayer) = 2 Then ResetSnakeApple : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 830 : pDMDLabelPulseImage3 "Lock1Anim",500,0 : pDMDmessage2L "Ball 1","Lock",1 : DOF 452, DOFPulse : if UseUltraDMD = 1 then UDMD "BALL LOCK 1", "", 3000
			If MBLock_Sav(CurrentPlayer) = 4 Then ResetSnakeApple : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 830 : pDMDLabelPulseImage3 "Lock2Anim",500,0 : pDMDmessage2L "Ball 2","Lock",1 : DOF 453, DOFPulse : if UseUltraDMD = 1 then UDMD "BALL LOCK 2", "", 3000
			If MBLock_Sav(CurrentPlayer) = 6 Then Mode_Multiball_Start : if UseUltraDMD = 1 then UDMD "MULTIBALL", "", 3000 
		CheckLightMB
		If DoorON = True Then DoorONTimer.Enabled = True
	Else
	DoorONTimer.Enabled = True
	End If
	Kicker004.timerinterval = 3000
	kicker004.timerenabled = True
end Sub
Sub Kicker004_Timer()
	'PlaySound "fx_kicker" 'burger
	SoundSaucerKick 1, Kicker004 'burger
	kicker004.kick 35, 50
    DOF 114, DOFPulse
	CheckDoorLock
'	If DoorON = True Then DoorONTimer.Enabled = True
	kicker004.timerenabled = False
End Sub

Sub Kicker005_Timer()
	'PlaySound "fx_kicker" 'burger
	playsoundat "warehousekick", Kicker005 'burger
    DOF 128, DOFPulse
	kicker005.kick -65, 10
	kicker005.timerenabled = False
End Sub
'***********
' Target
'***********
Sub AnimSnakeBG
	if Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 1 Then
		pDMDLabelWiggleImageEX "Snake2Anim", 500, 0, -10, 10
	End If
	if Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 2 Then
		pDMDLabelWiggleImageEX "Snake1Anim", 500, 0, -10, 10
	End If 
End Sub 

Sub Target001_Hit()
	AddScore 556
	Playsound "Target_Hit_2"
	StartAppel3
	If Snake3ok(CurrentPlayer) = 0 Then DOF 119, DOFPulse : DOF 448, DOFPulse 'Shaker + mx back explosion
	Snake3ok(CurrentPlayer) = 1
	AnimSnakeBG
End Sub

Sub Target002_Hit()
	AddScore 556
	Playsound "Target_Hit_2"
	StartAppel1
	If Snake1ok(CurrentPlayer) = 0 Then DOF 119, DOFPulse : DOF 448, DOFPulse 'Shaker + mx back explosion
	Snake1ok(CurrentPlayer) = 1
	AnimSnakeBG
End Sub

Sub Target003_Hit()
	AddScore 556
	Playsound "Target_Hit_2"
	StartAppel2
	If Snake2ok(CurrentPlayer) = 0 Then DOF 119, DOFPulse : DOF 448, DOFPulse 'Shaker + mx back explosion
	Snake2ok(CurrentPlayer) = 1
	AnimSnakeBG
End Sub

Sub Target004_Hit() 'Violet MiRamp Maji
	AddScore 556
	Playsound "Target_Hit_5"
	If LightRaisha001.State = 0 Then 
	LightRaisha001.State = 2
	Elseif LightRaisha001.State = 2 Then
	LightRaisha001.State = 1
	RaishaONTimer.enabled = True
	StartMiRaisha
	DOF 126, DOFPulse  'DOF - Gear Motor
	Playsound "Fx_Log" 
	End If
End Sub

Sub Target005_Hit() 'Rose MiBumper TAJI
	AddScore 556
	Playsound "Target_Hit_5"
	If LightNajwa001.State = 0 Then 
	LightNajwa001.State = 2
	Elseif LightNajwa001.State = 2 Then
	LightNajwa001.State = 1
	NajwaONTimer.enabled = True
	StartMiNajwa
	DOF 126, DOFPulse  'DOF - Gear Motor
	Playsound "Fx_Log"
	End If
End Sub

Sub Target006_Hit() 'Rouge MiSpinner NAJI
	AddScore 556
	Playsound "Target_Hit_5"
	If LightTasmine001.State = 0 Then 
	LightTasmine001.State = 2
	Elseif LightTasmine001.State = 2 Then
	LightTasmine001.State = 1
	TasmineONTimer.enabled = True
	StartMiTasmine
	DOF 126, DOFPulse  'DOF - Gear Motor
	Playsound "Fx_Log"
	End If
End Sub

Sub UpdateDMDHarem
	If PauseDMDHarem = 1 Then 
		OffDMDHarem
	Elseif PauseDMDHarem = 0 Then
		If RaishaActive = 0 And NajwaActive = 0 And TasmineActive = 0 Then OffDMDHarem
''		If (RaishaActive + NajwaActive + TasmineActive) = 0 or PauseDMDHarem = 1 Then OffDMDHarem
'		If RaishaActive = 1 And NajwaActive = 0 And TasmineActive = 0 Then pDMDLabelshow "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelHide "Harem007"
'		If RaishaActive = 1 And NajwaActive = 1 And TasmineActive = 0 Then pDMDLabelHide "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelshow "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelHide "Harem007"
'		If RaishaActive = 1 And NajwaActive = 0 And TasmineActive = 1 Then pDMDLabelHide "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelshow "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelHide "Harem007"
'		If RaishaActive = 1 And NajwaActive = 1 And TasmineActive = 1 Then pDMDLabelHide "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelshow "Harem007"
'		If RaishaActive = 0 And NajwaActive = 1 And TasmineActive = 0 Then pDMDLabelHide "Harem001" : pDMDLabelshow "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelHide "Harem007"
'		If RaishaActive = 0 And NajwaActive = 1 And TasmineActive = 1 Then pDMDLabelHide "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelHide "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelshow "Harem006" :pDMDLabelHide "Harem007"
'		If RaishaActive = 0 And NajwaActive = 0 And TasmineActive = 1 Then pDMDLabelHide "Harem001" : pDMDLabelHide "Harem002" :pDMDLabelshow "Harem003" :pDMDLabelHide "Harem004" :pDMDLabelHide "Harem005" :pDMDLabelHide "Harem006" :pDMDLabelHide "Harem007"
		If RaishaActive = 1 Then pDMDLabelShow "Miss1" Else	pDMDLabelHide "Miss1"
		If NajwaActive = 1 Then pDMDLabelShow "Miss2" Else	pDMDLabelHide "Miss2" 
		If TasmineActive = 1 Then pDMDLabelShow "Miss3" Else	pDMDLabelHide "Miss3" 

	End If
End Sub
		
Sub OffDMDHarem 'ou pause
'	pDMDLabelHide "Harem001" 
'	pDMDLabelHide "Harem002"
'	pDMDLabelHide "Harem003"
'	pDMDLabelHide "Harem004"
'	pDMDLabelHide "Harem005"
'	pDMDLabelHide "Harem006"
'	pDMDLabelHide "Harem007"
'pDMDLabelPulseImage "Miss1",500,1 
	pDMDLabelHide "Miss1"
	pDMDLabelHide "Miss2"
	pDMDLabelHide "Miss3"
	puPlayer.LabelSet pDMD,"HaremMi1Hit","" & FormatNumber(RaishaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi2Hit","" & FormatNumber(NajwaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi3Hit","" & FormatNumber(TasmineCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi1Time","" & FormatNumber(CountTimerValue1, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi2Time","" & FormatNumber(CountTimerValue2, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi3Time","" & FormatNumber(CountTimerValue3, 0),0,""
End Sub
'*****************
'Ramps MI Raisha
'*****************
Dim CountTimerValue1, CountTimerValue2, CountTimerValue3, PauseDMDHarem
CountTimerValue1 = 99
CountTimerValue2 = 75
CountTimerValue3 = 60
PauseDMDHarem = 0

Sub PauseDMDHaremTimer_Timer()
	PauseDMDHarem = 0
	PauseDMDHaremTimer.Enabled = False
End Sub

Sub CountTimerValue1Timer_Timer()
	CountTimerValue1 = CountTimerValue1 - 1
	If PauseDMDHarem = 0 and RaishaActive = 1 Then
		UpdateDMDHarem
		puPlayer.LabelSet pDMD,"HaremMi1Hit","" & FormatNumber(RaishaCount, 0),1,""
		puPlayer.LabelSet pDMD,"HaremMi1Time","" & FormatNumber(CountTimerValue1, 0),1,""
	Elseif PauseDMDHarem = 1 and RaishaActive = 1 Then
		OffDMDHarem
	Else
'		OffDMDHarem
		UpdateDMDHarem
	End If
End Sub
Sub CountTimerValue2Timer_Timer()
	CountTimerValue2 = CountTimerValue2 - 1
	If PauseDMDHarem = 0 and NajwaActive = 1 Then
		UpdateDMDHarem
		puPlayer.LabelSet pDMD,"HaremMi2Hit","" & FormatNumber(NajwaCount, 0),1,""
		puPlayer.LabelSet pDMD,"HaremMi2Time","" & FormatNumber(CountTimerValue2, 0),1,""
	Elseif PauseDMDHarem = 1 and NajwaActive = 1 Then
		OffDMDHarem
	Else
'		OffDMDHarem
		UpdateDMDHarem
	End If
End Sub
Sub CountTimerValue3Timer_Timer()
	CountTimerValue3 = CountTimerValue3 - 1
	If PauseDMDHarem = 0 and TasmineActive = 1 Then
		UpdateDMDHarem
		puPlayer.LabelSet pDMD,"HaremMi3Hit","" & FormatNumber(TasmineCount, 0),1,""
		puPlayer.LabelSet pDMD,"HaremMi3Time","" & FormatNumber(CountTimerValue3, 0),1,""
	Elseif PauseDMDHarem = 1 and TasmineActive = 1 Then
		OffDMDHarem
	Else
'		OffDMDHarem
		UpdateDMDHarem
	End If
End Sub

Sub StartMiRaisha
	DOF 335, DOFPulse
	MissionCount = MissionCount + 1
	RaishaActive = 1
	RaishaCount = 5
	UpdateDMDHarem
	'dmd
	if UseUltraDMD = 1 then UDMD "MAJI COLLECT", "ALL RAMPS", 3000
	'Playsound ""
	MiRaishaTimer.Enabled = True
	CountTimerValue1 = 99
	CountTimerValue1Timer.Enabled = True
	LightRaisha002.state = 2'Lumière RAMPS
	LightRaisha003.state = 2'Lumière RAMPS
	LightRaisha004.state = 2'Lumière RAMPS
	LightRaisha005.state = 2'Lumière RAMPS
	LightRaisha006.state = 2'Lumière RAMPS
End Sub

Sub MiRaishaTimer_Timer()
	If PauseDMDHarem = 0 Then
	pDMDLabelPulseText2 "HaremMi1Expireda", "Timer Maji",4000,clViolet
	pDMDLabelPulseText2 "HaremMi1Expiredb", "Expired",4000,clViolet
	End If
	FalledRaisha
End Sub

Sub FalledRaisha
	RaishaActive = 0
	RaishaCount = 5
	UpdateDMDHarem
	puPlayer.LabelSet pDMD,"HaremMi1Hit","" & FormatNumber(RaishaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi1Time","" & FormatNumber(CountTimerValue1, 0),0,""
	AddScore2 4500
	if UseUltraDMD = 1 then UDMD "MAJI MISSION", "FAILLED", 3000
	PlaySoundLooseMI
	MiRaishaTimer.Enabled = False
	CountTimerValue1Timer.Enabled = False
	LightRaisha002.state = 0'Lumière RAMPS
	LightRaisha003.state = 0'Lumière RAMPS
	LightRaisha004.state = 0'Lumière RAMPS
	LightRaisha005.state = 0'Lumière RAMPS
	LightRaisha006.state = 0'Lumière RAMPS
	If RaishaON = True Then RaishaONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightRaisha001.state = 0
End Sub

Sub AdwardRaisha
	If LightRaisha002.state = 1 And LightRaisha003.state = 1 And LightRaisha004.state = 1 And LightRaisha005.state = 1 And LightRaisha006.state = 1 Then
	RaishaActive = 0
	RaishaCount = 5
	UpdateDMDHarem
	PuPEvent 849
	AddMB1
	puPlayer.LabelSet pDMD,"HaremMi1Hit","" & FormatNumber(RaishaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi1Time","" & FormatNumber(CountTimerValue1, 0),0,""
	AddScore2 650000
	if UseUltraDMD = 1 then UDMD "MAJI RAMPS", "WIN 650K", 3000
	PlaySoundWinMI
	MiRaishaTimer.Enabled = False
	CountTimerValue1Timer.Enabled = False
	LightRaisha002.state = 0'Lumière RAMPS
	LightRaisha003.state = 0'Lumière RAMPS
	LightRaisha004.state = 0'Lumière RAMPS
	LightRaisha005.state = 0'Lumière RAMPS
	LightRaisha006.state = 0'Lumière RAMPS
	If RaishaON = True Then RaishaONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightRaisha001.state = 0
	End If
End Sub
'*****************
'Bumper MI Najwa
'*****************
Sub StartMiNajwa
	DOF 336, DOFPulse
	MissionCount = MissionCount + 1
	NajwaActive = 1
	NajwaCount = 25
	UpdateDMDHarem
	if UseUltraDMD = 1 then UDMD "TAJI COLLECT", "25 BUMPERS HIT", 3000
	MiNajwaTimer.Enabled = True
	CountTimerValue2 = 75
	CountTimerValue2Timer.Enabled = True
	LightNajwa002.state = 2'Lumière BUMPER
End Sub

Sub MiNajwaTimer_Timer()
	If PauseDMDHarem = 0 Then
	pDMDLabelPulseText2 "HaremMi2Expireda", "Timer Taji",4000,clPink
	pDMDLabelPulseText2 "HaremMi2Expiredb", "Expired",4000,clPink
	End If
	FalledNajwa
End Sub

Sub FalledNajwa
	NajwaActive = 0
	NajwaCount = 25
	UpdateDMDHarem
	puPlayer.LabelSet pDMD,"HaremMi2Hit","" & FormatNumber(NajwaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi2Time","" & FormatNumber(CountTimerValue2, 0),0,""
	AddScore2 4500
	if UseUltraDMD = 1 then UDMD "TAJI MISSION", "FAILLED", 3000
	PlaySoundLooseMI
	MiNajwaTimer.Enabled = False
	CountTimerValue2Timer.Enabled = False
	LightNajwa002.state = 0'Lumière BUMPER
	If NajwaON = True Then NajwaONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightNajwa001.state = 0
End Sub

Sub AdwardNajwa
	NajwaActive = 0
	NajwaCount = 25
	UpdateDMDHarem
	PuPEvent 849
	AddMB1
	puPlayer.LabelSet pDMD,"HaremMi2Hit","" & FormatNumber(NajwaCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi2Time","" & FormatNumber(CountTimerValue2, 0),0,""
	AddScore2 450000
	'dmd
	if UseUltraDMD = 1 then UDMD "TAJI BUMPER", "WIN 450K", 3000
	PlaySoundWinMI
	MiNajwaTimer.Enabled = False
	CountTimerValue2Timer.Enabled = False
	LightNajwa002.state = 0'Lumière BUMPER
	If TasmineON = True Then TasmineONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightNajwa001.state = 0
End Sub

'*****************
'Spinner MI Tasmine
'*****************
Sub Spinner001_Spin
    ' PlaySoundAt "fx_spinner", Spinner001 'burger
	SoundSpinner Spinner001 'burger
    If Tilted Then Exit Sub
    Addscore 750
	If TasmineActive = 1 Then
		TasmineCount = TasmineCount - 1
'		DispXText SpinnerCount
		If TasmineCount = 0 Then
			AdwardTasmine
		end If
	end If
End Sub

Sub StartMiTasmine
	MissionCount = MissionCount + 1
	DOF 334, DOFPulse
	TasmineActive = 1
	TasmineCount = 40
	UpdateDMDHarem
	'dmd
	if UseUltraDMD = 1 then UDMD "NAJI COLLECT", "40 SPINNERS", 3000
	'Playsound ""
	MiTasmineTimer.Enabled = True
	CountTimerValue3 = 60
	CountTimerValue3Timer.Enabled = True
	LightTasmine002.state = 2'Lumière spinner
End Sub

Sub MiTasmineTimer_Timer()
	If PauseDMDHarem = 0 Then
	pDMDLabelPulseText2 "HaremMi3Expireda", "Timer Naji",4000,clRed
	pDMDLabelPulseText2 "HaremMi3Expiredb", "Expired",4000,clRed
	End If
	FalledTasmine
End Sub

Sub FalledTasmine
	TasmineActive = 0
	TasmineCount = 40
	UpdateDMDHarem
	puPlayer.LabelSet pDMD,"HaremMi3Hit","" & FormatNumber(TasmineCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi3Time","" & FormatNumber(CountTimerValue3, 0),0,""
	AddScore2 4500
	if UseUltraDMD = 1 then UDMD "NAJI MISSION", "FAILLED", 3000
	PlaySoundLooseMI
	MiTasmineTimer.Enabled = False
	CountTimerValue3Timer.Enabled = False
	LightTasmine002.state = 0'Lumière ramp spinner
	If TasmineON = True Then TasmineONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightTasmine001.state = 0
End Sub

Sub AdwardTasmine
	TasmineActive = 0
	TasmineCount = 40
	UpdateDMDHarem
	PuPEvent 849
	AddMB1
	puPlayer.LabelSet pDMD,"HaremMi3Hit","" & FormatNumber(TasmineCount, 0),0,""
	puPlayer.LabelSet pDMD,"HaremMi3Time","" & FormatNumber(CountTimerValue3, 0),0,""
	AddScore2 350000
	'dmd
	if UseUltraDMD = 1 then UDMD "NAJI SPINNER", "WIN 350K", 3000
	PlaySoundWinMI
	MiTasmineTimer.Enabled = False
	CountTimerValue3Timer.Enabled = False
	LightTasmine002.state = 0'Lumière ramp spinner
	If TasmineON = True Then TasmineONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightTasmine001.state = 0
End Sub

Sub DebugHaremDMD
	if PauseDMDHarem = 0 And RaishaActive = 0 And NajwaActive = 0 And TasmineActive = 0 Then OffDMDHarem
End Sub
'**************
'Bonus Sultan
'**************
Sub StartMISultan
	ChangeBall(2) 'vert
	ChangePalmGreen
	AddScore 10000
	DOF 337, DOFPulse
	SultanActive = 1
	SultanONTimer.enabled = True
	PauseDMDHarem = 1
	PauseDMDHaremTimer.Enabled = True
	UpdateDMDHarem
	'PuPEvent
	if UseUltraDMD = 1 then UDMD "SULTAN COLLECT", "YOUR BONUS", 3000
	pDMDmessage2L "Sultan","Bonus Is Lit",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
'	GreenCount(CurrentPlayer) = 4
	BonusSultanCount(CurrentPlayer) = BonusSultanCount(CurrentPlayer) + 1
	CheckLiSultan
End Sub

Sub CheckSultanPupDMD
	If SultanActive = 1 Then
		If BonusSultanCount(CurrentPlayer) = 1 Then PuPEvent 860
		If BonusSultanCount(CurrentPlayer) = 3 Then PuPEvent 861
		If BonusSultanCount(CurrentPlayer) = 5 Then PuPEvent 862
		If BonusSultanCount(CurrentPlayer) = 7 Then PuPEvent 863
	End If
End Sub

Sub CheckLiSultan
		If BonusSultanCount(CurrentPlayer) = 0 Then Li1M.State = 0 : Li2M.State = 0 : Li3M.State = 0 : Li5M.State = 0 : ChangeBall(0) : ChangePalmBICOL
		If BonusSultanCount(CurrentPlayer) = 1 Then Li1M.State = 2 : Li2M.State = 0 : Li3M.State = 0 : Li5M.State = 0 : CheckSultanPupDMD
		If BonusSultanCount(CurrentPlayer) = 2 Then Li1M.State = 1 : Li2M.State = 0 : Li3M.State = 0 : Li5M.State = 0 : ChangeBall(0) : ChangePalmBICOL
		If BonusSultanCount(CurrentPlayer) = 3 Then Li1M.State = 1 : Li2M.State = 2 : Li3M.State = 0 : Li5M.State = 0 : CheckSultanPupDMD
		If BonusSultanCount(CurrentPlayer) = 4 Then Li1M.State = 1 : Li2M.State = 1 : Li3M.State = 0 : Li5M.State = 0 : ChangeBall(0) : ChangePalmBICOL
		If BonusSultanCount(CurrentPlayer) = 5 Then Li1M.State = 1 : Li2M.State = 1 : Li3M.State = 2 : Li5M.State = 0 : CheckSultanPupDMD
		If BonusSultanCount(CurrentPlayer) = 6 Then Li1M.State = 1 : Li2M.State = 1 : Li3M.State = 1 : Li5M.State = 0 : ChangeBall(0) : ChangePalmBICOL
		If BonusSultanCount(CurrentPlayer) = 7 Then Li1M.State = 1 : Li2M.State = 1 : Li3M.State = 1 : Li5M.State = 2 : CheckSultanPupDMD
		If BonusSultanCount(CurrentPlayer) >= 8 Then Li1M.State = 1 : Li2M.State = 1 : Li3M.State = 1 : Li5M.State = 1 : ChangeBall(0) : ChangePalmBICOL
End Sub

Sub AwardMISultan 
	If BonusSultanCount(CurrentPlayer) = 2 Then ChangeBall(0) : ChangePalmBICOL : AddScore2 1000000 : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 864 : pDMDmessage2L "Sultan Bonus","1 Million",1 : DOF 450, DOFPulse : if UseUltraDMD = 1 then UDMD "SULTAN", "BONUS 1M", 3000
	If BonusSultanCount(CurrentPlayer) = 4 Then ChangeBall(0) : ChangePalmBICOL : AddScore2 2000000 : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 865 : pDMDmessage2L "Sultan Bonus","2 Million",1 : DOF 450, DOFPulse : if UseUltraDMD = 1 then UDMD "SULTAN", "BONUS 2M", 3000
	If BonusSultanCount(CurrentPlayer) = 6 Then ChangeBall(0) : ChangePalmBICOL : AddScore2 3000000 : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 866 : pDMDmessage2L "Sultan Bonus","3 Million",1 : DOF 450, DOFPulse : if UseUltraDMD = 1 then UDMD "SULTAN", "BONUS 3M", 3000
	If BonusSultanCount(CurrentPlayer) = 8 Then ChangeBall(0) : ChangePalmBICOL : AddScore2 5000000 : PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 867 : pDMDmessage2L "Sultan Bonus","5 Million",1 : DOF 450, DOFPulse : if UseUltraDMD = 1 then UDMD "SULTAN", "BONUS 5M", 3000
	SultanActive = 0
	If SultanON = True Then SultanONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	PlaySoundWinMI
	PieceCount(CurrentPlayer) = 0
	PauseDMDHarem = 1
	PauseDMDHaremTimer.Enabled = True
	UpdateDMDHarem
	'PuPEvent
	CheckPieceCount
	CheckLiSultan
End Sub

Sub EndMISultan
	SultanActive = 0
	ChangeBall(0) 'off
	ChangePalmBICOL
	If BonusSultanCount(CurrentPlayer) = 1 or BonusSultanCount(CurrentPlayer) = 3 or BonusSultanCount(CurrentPlayer) = 5 or BonusSultanCount(CurrentPlayer) = 7 Then 
		BonusSultanCount(CurrentPlayer) = BonusSultanCount(CurrentPlayer) - 1
	End If
	If SultanON = True Then SultanONTimer.Enabled = True : Playsound "Fx_Log" : PieceCount(CurrentPlayer) = PieceCount(CurrentPlayer) - 1 : DOF 126, DOFPulse  'DOF - Gear Motor
	CheckPieceCount
	CheckLiSultan
End Sub
'*****************
'Spinner LAMP & Mystery GENIE
'*****************
sub timer1_Timer()
	If MysteryState(CurrentPlayer) <= 1 And LampSoutCollected(CurrentPlayer) = 2 Then
		LampPr002.rotY=rainspin1.currentangle
		LampSupport.rotY=rainspin1.currentangle
	Elseif MysteryState(CurrentPlayer) <= 1 And LampSoutCollected(CurrentPlayer) <= 1 Then
		LampSupport.rotY=rainspin1.currentangle
	Else
		LampPr002.RotY = -20
		LampSupport.RotY = -20
	End If
end Sub

Sub Rainspin1_Spin
    If Tilted Then Exit Sub
    Addscore 175
	If ModeSpinnersActive(CurrentPlayer) = true Then
		SpinnerCount(CurrentPlayer) = SpinnerCount(CurrentPlayer) - 1
'		DispXText SpinnerCount
		If SpinnerCount(CurrentPlayer) <= 0 And BallsOnPlayfield = 1 Then
			ModeSpinnersActive(CurrentPlayer) = False
			MysteryState(CurrentPlayer) = 2
			Light010.state = 0
			MysteryActivedTimer.enabled = True
		end If
	end If
	If Light009.State=2 And BallsOnPlayfield = 1 Then
		TopMagnet.MagnetOn = True 
		StartModeChosenMystery
	End If
	'If MysteryActivedTimer.enabled = False Then PlaySoundAt "fx_spinner", rainspin1 'burger
	If MysteryActivedTimer.enabled = False Then SoundSpinner rainspin1 'burger
	If RaishaActive = 1 And LightRaisha002.state = 2 Then LightRaisha002.state = 1 :RaishaCount = RaishaCount - 1 : AdwardRaisha
End Sub

Sub MysteryActivedTimer_Timer()
	LampPr002.RotY = -20
	LampSupport.RotY = -20
	Light009.state = 2
	ChangeBall(4) 'jaune
	ChangePalmYel
	MysteryActivedTimer.enabled=False
End Sub

Sub StartModeChosenMystery
	ResetGirl
	Light009.State=1
'	bMysterySelect = True
	DOF 339, DOFPulse
	StartFumee
	If usePuPDMD = False Then 
		PlaySoundWishStart
		bMysterySelect = True
		UpdateDMDMystery 
	Else 
		PuPEvent 880
		vpmTimer.AddTimer 10000, "bMysterySelect = True '"
		vpmTimer.AddTimer 10000, "UpdateDMDMystery '"
	End If
	if UseUltraDMD = 1 then UDMD "SELECT WISHE", "< >", 3000
End Sub

Sub ResetMystery
	Light009.State=0
	Light010.State=2
	ChangeBall(0) 'off
	ChangePalmBICOL
	MysteryState(CurrentPlayer) = 1
	ModeSpinnersActive(CurrentPlayer) = true
	SpinnerCount(CurrentPlayer) = 15
	vpmTimer.AddTimer 7000, "TopMagnet.MagnetOn = False '"
End Sub

'----MysteryWhich
Dim Mystery, Mysterynr, LiberationLock
Mystery = ""
Mysterynr = INT(RND * 3)
LiberationLock = True

Sub PlayRandomMystery
    Mysterynr = INT(RND * 3)
    PLaySelectedMystery
End Sub

Sub PLaySelectedMystery
	Light009.State=1
	If usePuPDMD = False Then PlaySoundWish
'	PauseDMDHaremTimer.Enabled = True
    Select Case Mysterynr
        Case 0 ' Devenir Prince bonus x2
			AladdinPersoState(CurrentPlayer) = 1
			vpmTimer.AddTimer 5000, "CheckAladdinPersoState '"
			PuPEvent 881
			if UseUltraDMD = 1 then UDMD "PRINCE ALI", "BONUS X2", 3000	
        Case 1 ' TOUTE LES POMMES2
			AppleCount(CurrentPlayer) = 6
			CheckAppleCount
			PuPEvent 882
			if UseUltraDMD = 1 then UDMD "ALL APPLES", "ADDED", 3000
        Case 2 'ADD 1 BALL
			AddMB1
			PuPEvent 883
			if UseUltraDMD = 1 then UDMD "1 BALL", "ADDED", 3000	
        Case 3 'ADD 1 COEUR JASMINE
			JasmineCount(CurrentPlayer) = JasmineCount(CurrentPlayer) + 1
			pDMDLabelPulseImage2 "CoeurAnim",2000,0
			pDMDLabelPulseText2 "CoeurAdd","+1",2000,clWhite
			SelectAwardJasmineALT
			PuPEvent 884
			UpDateBGimage
			if UseUltraDMD = 1 then UDMD "JASMINE FAVOR", "ADDED", 3000
        Case 4 ' DEVENIR GUERRIER PLAYFIELD X2
			PuPEvent 885
			AladdinPersoState(CurrentPlayer) = 2
			vpmTimer.AddTimer 5000, "CheckAladdinPersoState '"
			if UseUltraDMD = 1 then UDMD "ALADDIN WARRIOR", "PLAYFIELD X2", 3000
		Case 5 'TOUTE LES PIECES ET START MI SULTAN
			PieceCount(CurrentPlayer) = 6
			CheckPieceCount
			if UseUltraDMD = 1 then UDMD "MISSION", "SULTAN BONUS", 3000
		Case 6 ' LIBERER GENIE
			PuPEvent 886
			if UseUltraDMD = 1 then UDMD "GENIE", "UNLEASH", 3000
			StartGenieFree
    End Select
	DOF 340, DOFPulse
	PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub SelectMystery(keycode)
    If keycode = PlungerKey OR keycode = StartGameKey Then
		If LiberationLock = False Then 
			PLaySelectedMystery
			bMysterySelect = False
			Genie.visible = 0
			ResetMystery
		Else 
			Playsound "Fx_Unlock"
		End If
    End If
    If keycode = LeftFlipperKey Then
        Mysterynr = (Mysterynr - 1)
        If Mysterynr <0 Then Mysterynr = 6
        UpdateDMDMystery
    End If
    If keycode = RightFlipperKey Then
        Mysterynr = (Mysterynr + 1)MOD 7
        UpdateDMDMystery
    End If
End Sub

Sub UpdateDMDMystery() 'Updates the DMD with the chosen
'    DMDFlush
	PauseDMDHarem = 1
    Select Case Mysterynr
        Case 0
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "MAKE ME", "A PRINCE", 3000 
        Case 1
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "STOCK UP", "ALL APPLES", 3000 
        Case 2
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "ADD", "1 BALL", 3000
        Case 3
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "ADD JASMINE", "FAVOR", 3000
        Case 4
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "MAKE ME", "A WARRIOR", 3000
		Case 5
			LiberationLock = False
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "START MISSION", "BONUS SULTAN", 3000
        Case 6
			If StoryCount(CurrentPlayer) = 11 Then LiberationLock = False Else LiberationLock = True
			PuPlayer.LabelSet pDMD,"WishGeni","PUPAlphas\\Wish007b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
			if UseUltraDMD = 1 then UDMD "UNLEASH", "GENIE", 3000
    End Select
End Sub

'************
' Multiball
'************
Sub CheckLightMB
	If MBLock_Sav(CurrentPlayer) = 0 Then MBLockActive(CurrentPlayer) = False : LiLOCK.State = 0 : LiMB.State = 0
	If MBLock_Sav(CurrentPlayer) = 1 Then MBLockActive(CurrentPlayer) = True : LiLOCK.State = 2 : LiMB.State = 0
	If MBLock_Sav(CurrentPlayer) = 2 Then MBLockActive(CurrentPlayer) = False : LiLOCK.State = 0 : LiMB.State = 0 
	If MBLock_Sav(CurrentPlayer) = 3 Then MBLockActive(CurrentPlayer) = True : LiLOCK.State = 2 : LiMB.State = 0
	If MBLock_Sav(CurrentPlayer) = 4 Then MBLockActive(CurrentPlayer) = False : LiLOCK.State = 0 : LiMB.State = 0 
	If MBLock_Sav(CurrentPlayer) = 5 Then MBLockActive(CurrentPlayer) = True : LiLOCK.State = 0 : LiMB.State = 2
	If MBLock_Sav(CurrentPlayer) >= 6 Then MBLockActive(CurrentPlayer) = False : LiLOCK.State = 1 : LiMB.State = 1
	UpDateBGimage
End Sub

Sub Mode_Multiball_Start
	ChangeGi red
	ChangeBall(3) 'red
	ChangePalmRed
	DOF 324, DOFOn
	pDMDmessage2L "Multiball","Start",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	if UseUltraDMD = 1 then UDMD "START", "MULTIBALL", 3000
	MBLock_Sav(CurrentPlayer) = 6
	PlaySong "Mu_MB"
	PauseDMDHarem = 1
'	PuPEvent 806
'	PuPEvent 831
	AddScore2 115000
	bMultiBallMode = True
	EnableBallSaver 15
'	EnableBallSaver BallSaverTime
	vpmtimer.addtimer 3000, "AddMB1 '"
	vpmtimer.addtimer 6000, "AddMB1 '"
'	BallsOnPlayfield = 3
End Sub

Sub Mode_Multiball_End 
	ResetSnakeApple
	ChangeBall(0) 'off
	ChangePalmBICOL
	AddScore2 35000
	DOF 324, DOFOff
	bMultiBallMode = False
	ChangeGi white
'	PuPEvent 816
	MBLock_Sav(CurrentPlayer) = 0
	MBLockActive(CurrentPlayer) = False
	CheckLightMB
End Sub

Sub AddMB1
	LightEffect 4
	KickerAuto.CreateSizedBallWithMass BallSize / 2, BallMass
	KickerAuto.Kick 10, 1
	DOF 316, DOFPulse
    BallsOnPlayfield = BallsOnPlayfield + 1 
    PlaySoundAt SoundFXDOF("", 120, DOFPulse, DOFContactors), BallRelease 'burger
	RandomSoundBallRelease Ballrelease 'burger
bAutoPlunger = True
End Sub
'***********
'  Fumée
'***********
Dim FumeePos, FramesFumee
FramesFumee = Array("fumee10", "fumee11", "fumee12", "fumee13", "fumee14", "fumee15", "fumee16", "fumee17", "fumee18", "fumee19", "fumee20", "fumee21", "fumee22", "fumee23", "fumee24", "fumee25", "fumee26", "fumee27", "fumee28", "fumee29") 
FumeePos = 0

Sub StartFumee
	Fumee1.visible = 1
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	Fumeetimer.Enabled = True
	fumeeOFFtimer.Enabled = True
End Sub

Sub Fumeetimer_timer
    Fumee1.ImageA = FramesFumee (FumeePos)
    FumeePos = (FumeePos + 1) MOD 20
End Sub

Sub fumeeOFFtimer_timer
	Fumee1.visible = 0
	FlashBotelL.Visible = 0
	FlashBotelR.Visible = 0
	F1A008.state = 0
	F1A005.state = 0
	Genie.visible = 1
	Fumeetimer.Enabled = False
	fumeeOFFtimer.Enabled = False
End Sub
'*****************
'Triggers
'*****************

Sub Trigger004_Hit()
	JafaSnakeTimer.enabled=True
End Sub

Sub Trigger005_Hit()
	IagoTimer.enabled=True
	PlaySoundParrot
	FlashForMs F1A003, 1000, 50, 0
	If AppleCount(CurrentPlayer) <= 5 Then Apple002.visible = 1 : Apple003.visible = 1
End Sub

Sub Trigger006_Hit()
	DOF 441, DOFPulse
	AddScore 750
	RampCount = RampCount + 1
	If SultanActive = 0 Then
		If PieceCount(CurrentPlayer) <= 5 And AladdPiece.visible = True Then
		PieceCount(CurrentPlayer) = PieceCount(CurrentPlayer) + 1
		pDMDLabelPulseText2 "PieceAdd","+1",2000,clGreen
		Playsound "Fx_Bonus1"
		AladdPiece.visible = 0
		AladpieceRottimer.enabled=False
		CheckPieceCount
		End If
	Elseif SultanActive = 1 Then
		'And (Li1M.state = 2 or Li2M.state = 2 or Li3M.state = 2 or Li5M.state = 2) Then 
		BonusSultanCount(CurrentPlayer) = BonusSultanCount(CurrentPlayer) + 1 
		AwardMISultan
	End If
	If RaishaActive = 1 And LightRaisha005.state = 2 Then LightRaisha005.state = 1 : RaishaCount = RaishaCount - 1 : AdwardRaisha
End Sub

Sub Trigger007_Hit()
	AddScore 750
	DOF 481, DOFPulse
	RampCount = RampCount + 1
	If RaishaActive = 1 And LightRaisha006.state = 2 Then LightRaisha006.state = 1 : RaishaCount = RaishaCount - 1 : AdwardRaisha
	AwardJackpot
End Sub

Sub TLeftOutlane_Hit() '
	AddScore 250
	DOF 309, DOFPulse
	If LeftOutlane.State = 0 Then
	LeftOutlane.State = 2
	Elseif LeftOutlane.State = 2 Then
'	DOF 352, DOFPulse
	LeftOutlane.State=1
	CheckLightLANE
	End If
End Sub

Sub TLeftInlane_Hit() '
	AddScore 250
	DOF 311, DOFPulse
	leftInlaneSpeedLimit
	If SultanActive = 0 And PieceCount(CurrentPlayer) <= 5 Then AladpieceRottimer.enabled=True
	If LeftInlane.State = 0 Then
	LeftInlane.State = 2
	Elseif LeftInlane.State = 2 Then
	LeftInlane.State=1
	CheckLightLANE
	End If
End Sub

Sub TRightInlane_Hit() '
	AddScore 250
	DOF 312, DOFPulse
	rightInlaneSpeedLimit
	If RightInlane.State = 0 Then
	RightInlane.State = 2
	Elseif RightInlane.State = 2 Then
	RightInlane.State=1
	CheckLightLANE
	End If
End Sub

Sub TRightOutlane_Hit() '
	DOF 310, DOFPulse
	AddScore 250
	If RightOutlane.State = 0 Then
	RightOutlane.State = 2
	Elseif RightOutlane.State = 2 Then
	RightOutlane.State=1
	CheckLightLANE
	End If
End Sub

Sub CheckLightLANE
	If LeftOutlane.State=1 And LeftInlane.State=1 And RightInlane.State=1 And RightOutlane.State=1 Then
	AddScore 8500
	Playsound "Fx_Bonus1"
	DOF 338, DOFPulse
	KickBackActive(CurrentPlayer) = True
	End If
End Sub

Sub TriggerDebug_Hit()
	If BallsOnPlayfield = 0 Then TriggerDebug.DestroyBall
End Sub

Sub TriggerTEST_Hit()
	If IntroTpsOn = 1 Then PlaySong "Mu_1" : StopFlasherApron : Pupevent 857 : IntroTpsOn = 0
'	ModeSablier = True
'	ModeSablierTimer.Enabled = True
'	LancerRoueAladdin
'	AddScore 1000000
'	HighScorelabels
'	StartModeSnake
'	StartMISultan
'	StartBoss
'	ChangeBall(0) 'off
'	ChangeBall(1) 'ok Bleu Effet mode Snake
'	ChangeBall(2) 'ok vert mode Sultan
'	ChangeBall(3) 'ok rouge mode MB et Sablier
'	ChangeBall(4) 'ok jaune Genie is lit / Lamp Souterrain ok
'	pdmdlabelshow "Harem007"
'	pDMDScrollBigV "pupStatus", "BALL SAVED",3,clRed 'Ok defilement de bas vers haut
'	pDMDScrollBig "pupStatus", "BALL SAVED",3,clRed 'Ok defilement de droite vers gauche
'	pDMDZoomBig "pupStatus", "BALL SAVED", 3, clRed', 'ok
'	pDMDZoomAndHide "pupStatus", "BALL SAVED", 3, clRed, 70, 20 'Ok zoom 70% vers normal et disparait
'	pStatus "BALL SAVED" 'fixe Reste
'	pStatus2 "BALL SAVED", 3000, clRed 'ok clignote et reste
'	pStatus2AndHide "BALL SAVED", 3000, clRed 'ok clignote et s'efface
'	pStatus3AndHide "BALL SAVED", 3000, clRed 'ok Fixe et s'efface
'	pStatusHarem "HaremMi1Expired","Timer Expired", 3000, clRed
'	pDMDScrollBig "HaremMi1Expireda", "Timer Maji",4,clViolet 
'	pDMDScrollBig "HaremMi1Expiredb", "Expired",4,clViolet
'	pDMDScrollBig "HaremMi2Expireda", "Timer Taji",4,clPink
'	pDMDScrollBig "HaremMi2Expiredb", "Expired",4,clPink
'	pDMDScrollBig "HaremMi3Expireda", "Timer Naji",4,clRed
'	pDMDScrollBig "HaremMi3Expiredb", "Expired",4,clRed
'	pDMDScrollBigV "HaremMi1Expireda", "Timer Maji",4,clViolet 
'	pDMDScrollBigV "HaremMi1Expiredb", "Expired",4,clViolet
'	pDMDScrollBigV "HaremMi2Expireda", "Timer Taji",4,clPink
'	pDMDScrollBigV "HaremMi2Expiredb", "Expired",4,clPink
'	pDMDScrollBigV "HaremMi3Expireda", "Timer Naji",4,clRed
'	pDMDScrollBigV "HaremMi3Expiredb", "Expired",4,clRed
'	pDMDLabelPulseText2 "HaremMi1Expireda", "Timer Maji",4000,clViolet 
'	pDMDLabelPulseText2 "HaremMi1Expiredb", "Expired",4000,clViolet
'	pDMDLabelPulseText2 "HaremMi2Expireda", "Timer Taji",4000,clPink
'	pDMDLabelPulseText2 "HaremMi2Expiredb", "Expired",4000,clPink
'	pDMDLabelPulseText2 "HaremMi3Expireda","Timer Naji",4000,clRed
'	pDMDLabelPulseText2 "HaremMi3Expiredb","Expired",4000,clRed
'no	pDMDZoomBig "HaremMi3Expireda","Timer Naji",4000,clRed
'no	pDMDZoomBig "HaremMi3Expiredb","Expired",4000,clRed
'ok	pDMDLabelPulseImage2 "CoeurAnim",2000,0
'ok	pDMDLabelPulseText2 "CoeurAdd","+1",2000,clWhite
'ok	pDMDLabelPulseText2 "PieceAdd","+1",2000,clGreen
'ok	pDMDLabelPulseText2 "Bumper001","Bim",1000,clWhite
'ok	pDMDLabelPulseText2 "Bumper002","Bam",1000,clWhite
'ok	pDMDLabelPulseText2 "Bumper003","Boum",1000,clWhite
'-------------------
'pDMDLabelShow "Lock1Anim" 
'pDMDLabelShow "Lock2Anim" 
'pDMDLabelShow "Snake1Anim" 
'pDMDLabelShow "Snake2Anim" 
'pDMDLabelShow "PieceAnim"
'ok pDMDLabelPulseImage3 "Lock1Anim",500,0
'ok pDMDLabelPulseImage3 "Lock2Anim",500,0
'ok pDMDLabelWiggleImageEX "Snake1Anim", 500, 0, -10, 10
'ok pDMDLabelWiggleImageEX "Snake2Anim", 500, 0, -10, 10
'pDMDLabelShow "Miss1" 
'pDMDLabelShow "Miss2"
'pDMDLabelShow "Miss3"
'pDMDLabelPulseImage "Miss1",500,1
'pDMDLabelPulseImage "Miss2",500,1
'pDMDLabelPulseImage "Miss3",500,1
'puPlayer.LabelSet pDMD,"HaremMi1Hit","" & FormatNumber(RaishaCount, 0),1,""
'puPlayer.LabelSet pDMD,"HaremMi2Hit","" & FormatNumber(NajwaCount, 0),1,""
'puPlayer.LabelSet pDMD,"HaremMi3Hit","" & FormatNumber(TasmineCount, 0),1,""
'puPlayer.LabelSet pDMD,"HaremMi1Time","" & FormatNumber(CountTimerValue1, 0),1,""
'puPlayer.LabelSet pDMD,"HaremMi2Time","" & FormatNumber(CountTimerValue2, 0),1,""
'puPlayer.LabelSet pDMD,"HaremMi3Time","" & FormatNumber(CountTimerValue3, 0),1,""
'----------------------
'	pDMDLabelShow "CoeurAnim"
'	AladdinNormal.Visible = 0
'	AladdinGuerrier.Visible = 1
'	Apple001.Visible = 0
'	AladdinPrince.Visible = 0
'	AwardExtraBall
'	ChangeBall(3)
'	ChangePalmGreen
'	ChangePalmRed
'	ChangePalmBlue
'	ChangePalmPink
'	ChangePalmYel
'	ChangePalmBICOL
'	PuPEvent 821
'	TorcheB001.Visible = 1
	' Augmenter l'intensité pour un Glow prononcé
'	TorcheB002.Light.Intensity = 500000 
'	TorcheB002.Intensity = 500000 
' Rendre la lumière moins "tapered" (moins de déclin rapide) pour un Glow plus visible
'	TorcheB002.Light.Falloff = 1 
'	GiOff
'	IagoTimer.enabled=True
'	table1.ColorGradeImage = "ColorGradeLUT256x16_1to4"
'	ChangeGi white
'	TopMagnetL.MagnetOn = True
'	TopMagnetR.MagnetOn = True
'	EnterPOA.TimerEnabled=1
' 	StartMegaTapi
'	StartFumee
'	MonkeyRTimer.enabled=True
'	JafaSnakeTimer.enabled=True
'	JafaCannTimer.enabled=True
'	StartAppel1
'	Apple3Timer.enabled=True
'	StoryCount(CurrentPlayer) = 7
'	CheckStoryCount
'TEST GENIE -----
'	LampSouterrain(CurrentPlayer) = 11
'	LampSoutCollected(CurrentPlayer) = 2
'	MysteryState(CurrentPlayer) = 2
'	MysteryActivedTimer.enabled = True
'	Light008.State = 0
'	Light009.State = 2
'	CheckModeCaverne	
'	Light010.State = 0
'	StoryCount(CurrentPlayer) = 6
'	CheckStoryCount
''-----------------
'	CoeurCount(CurrentPlayer) = 3
'	CheckCoeurCount
'	TasmineONTimer.enabled=True
'	PuPlayer.LabelSet pDMD,"HighScore","Enter Your Name :",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL2","A",1,""
'	PuPlayer.LabelSet pDMD,"HighScoreL3","A",1,""
'	LightCityON
'	GiEffect 4
'	LightEffect 2
'	TasmineONTimer.enabled=True
'	SultanONTimer.enabled=True
'	NajwaONTimer.enabled=True
'	RaishaONTimer.enabled=True
'	DoorONTimer.enabled=True
End Sub

Sub AnimTrailerTimer_Timer()
	LampPr002.Visible = 1
	AppleCount(CurrentPlayer) = 6
'	Cave.Visible = 1
'	Cave001.Visible = 1
			Dim ModeChosenAnimation
		ModeChosenAnimation = False
		Do While ModeChosenAnimation = False
			Select Case RndNbr(13)
			Case 1
				SultanONTimer.enabled=True
				ModeChosenAnimation = True
			Case 2
				TasmineONTimer.enabled=True
				ModeChosenAnimation = True
			Case 3
				NajwaONTimer.enabled=True
				ModeChosenAnimation = True
			Case 4
				RaishaONTimer.enabled=True
				ModeChosenAnimation = True
			Case 5
				StartAppel1
			Case 6
				StartAppel2
			Case 7
				StartAppel3
			Case 8
				DoorONTimer.enabled=True
			Case 9
				StartFumee
				ModeChosenAnimation = True
			Case 10
				Genie.visible = 0
			Case 11
				JafaSnakeTimer.enabled=True
			Case 12
				JafaCannTimer.enabled=True
				ModeChosenAnimation = True
			Case 13
				IagoTimer.enabled=True
				ModeChosenAnimation = True
			End Select
		Loop
'	Else
'	End If
End Sub

'***********
'  Eau
'***********
Dim EauPos, FramesEau
FramesEau = Array("Eau_000", "Eau_001", "Eau_002", "Eau_003", "Eau_004", "Eau_005", "Eau_006") 
EauPos = 0
Sub Eautimer_timer
    Eau1.ImageA = FramesEau (EauPos)
    EauPos = (EauPos + 1) MOD 7
End Sub
'#################################
'FX SOUND
'#################################
Sub PlaySoundLockIsLit
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Vo_GoodApu001"
		Case 2
			PlaySound "Vo_GoodApu002"
		Case 3
			PlaySound "Vo_GoodApu003"
	End Select
End Sub

Sub PlaySoundLooseMI
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Vo_LooseMi001"
		Case 2
			PlaySound "Vo_LooseMi002"
		Case 3
			PlaySound "Vo_LooseMi003"
	End Select
End Sub

Sub PlaySoundWinMI
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "Fx_WinMi001"
		Case 2
			PlaySound "Fx_WinMi002"
		Case 3
			PlaySound "Fx_WinMi003"
		Case 4
			PlaySound "Fx_WinMi004"
	End Select
End Sub

Sub PlaySoundJump
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "fx_Jump001"
		Case 2
			PlaySound "fx_Jump002"
		Case 3
			PlaySound "fx_Jump003"
		Case 4
			PlaySound "fx_Jump004"
	End Select
End Sub

Sub PlaySoundWishStart
	Select Case Int(Rnd * 2) + 1
		Case 1
			PlaySound "Vo_SpinTurnGenie"
		Case 2
			PlaySound "Vo_GenieStart"
	End Select
End Sub

Sub PlaySoundWish
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Vo_Wish001"
		Case 2
			PlaySound "Vo_Wish002"
		Case 3
			PlaySound "Vo_Wish003"
	End Select
End Sub

Sub PlaySoundThunder
	Select Case Int(Rnd * 3) + 1
		Case 1
			PlaySound "Fx_Thunder1"
		Case 2
			PlaySound "Fx_Thunder2"
		Case 3
			PlaySound "Fx_Thunder3"
	End Select
End Sub

Sub PlaySoundLock
	Select Case Int(Rnd * 4) + 1
		Case 1
			PlaySound "Fx_Lock001"
		Case 2
			PlaySound "Fx_Lock002"
		Case 3
			PlaySound "Fx_Lock003"
		Case 4
			PlaySound "Fx_Lock004"
	End Select
End Sub

Sub PlaySoundEND
	PauseDMDHaremTimer.Enabled = False
	CountTimerValue1Timer.Enabled = False
	CountTimerValue2Timer.Enabled = False
	CountTimerValue3Timer.Enabled = False
	OffDMDHarem
	PauseDMDHarem = 0
	Select Case Int(Rnd * 3) + 1
		Case 1
			If usePuPDMD = False Then PlaySound "Vo_End002"	Else PuPEvent 838
		Case 2
			If usePuPDMD = False Then PlaySound "Vo_End003"	Else PuPEvent 838
		Case 3
			If usePuPDMD = False Then PlaySound "Vo_End004"	Else PuPEvent 838
	End Select
End Sub

Sub PlaySoundParrot
	Select Case Int(Rnd * 7) + 1
		Case 1
			PlaySound "Vo_Parrot001"
		Case 2
			PlaySound "Vo_Parrot002"
		Case 3
			If usePuPDMD = False Then PlaySound "Vo_Parrot003" Else PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 887
		Case 4
			PlaySound "Vo_Parrot004"
		Case 5
			PlaySound "Vo_Parrot005"
		Case 6
			If usePuPDMD = False Then PlaySound "Vo_Parrot006"	Else PauseDMDHarem = 1 : PauseDMDHaremTimer.Enabled = True : UpdateDMDHarem : PuPEvent 887
		Case 7
			PlaySound "Vo_Parrot007"
	End Select
End Sub

Sub PlaySoundBoss3
	Select Case Int(Rnd * 2) + 1
		Case 1
			PlaySound "fx_Boss3001"
		Case 2
			PlaySound "fx_Boss3002"
	End Select
End Sub

'*************
' Save CurrentPlayer
'*************
Sub ResetEvents
	table1.ColorGradeImage = "ColorGradeLUT256x16_1to2"
	ResetBumpers
	SlingCount = 0 'Bonus 1
	RampCount = 0 'bonus 2
	BumperCount = 0 'bonus 3
	MissionCount = 0 'bonus 4
	AladdinPersoState(CurrentPlayer) = 0
	CheckAladdinPersoState
	StoryCount(CurrentPlayer) = 0
	CheckStoryCount
	JafaScoopCount = 0
	LampSouterrain(CurrentPlayer) = 0
	LampSoutCollected(CurrentPlayer) = 0
	Light008.State = 2
	bModeSouterrainActive = False
	ModeMegaTapisActive = False
	SpinnerCount(CurrentPlayer) = 8
	ModeSpinnersActive(CurrentPlayer) = False
'	AppleCount(CurrentPlayer) = 3
	AppleCount(1) = 3
	AppleCount(2) = 3
	AppleCount(3) = 3
	AppleCount(4) = 3
'	PieceCount(CurrentPlayer) = 1
	PieceCount(1) = 1
	PieceCount(2) = 1
	PieceCount(3) = 1
	PieceCount(4) = 1
	CheckPieceCount
	CheckAppleCount
	Snake1ok(CurrentPlayer) = 0
	Snake2ok(CurrentPlayer) = 0
	Snake3ok(CurrentPlayer) = 0
	MBLock_Sav(CurrentPlayer) = 0
	BonusSultanCount(CurrentPlayer) = 0
	ResetSultanGirls
	BonusSultanCount(CurrentPlayer) = 0
	JasmineCount(CurrentPlayer) = 0
	LifeBoss2count = 0
	LifeAladdCount = 0
	ResetCoeurCount
	CheckModeCaverne
	EndMegaTapi
	LeftOutlane.State=1
	LeftInlane.State=1
	RightInlane.State=1
	RightOutlane.State=1
	KickBackActive(CurrentPlayer) = True
	ModeSablier = False
	ModeSablierTimer.Enabled = False
	If DoorON = True Then DoorONTimer.Enabled = True
'	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	UpDateBGimage
	If CheatStoryLevel >=1 Then LoadStoryLevel
	bModeGenieFree = False
	JumpCount = 0
	JackpotValue(CurrentPlayer) = 0
End Sub
	
Sub CheckEventPlayer
	ResetBumpers
	JumpCount = 0
	SlingCount = 0 'Bonus 1
	RampCount = 0 'bonus 2
	BumperCount = 0 'bonus 3
	MissionCount = 0 'bonus 4
	CheckAladdinPersoState
	ResetSultanGirls
	CheckAppleCount
	CheckPieceCount
	CheckCoeurCount
	CheckSnakeOK
	CheckModeCaverne
	CheckStoryCount
	ModeMegaTapisActive = False
	EndMegaTapi
	LeftOutlane.State=1
	LeftInlane.State=1
	RightInlane.State=1
	RightOutlane.State=1
	KickBackActive(CurrentPlayer) = True
	ModeSablier = False
	ModeSablierTimer.Enabled = False
'	JafaScoopCount = 0
	CheckDoorLock
'	If B2SOn Then Controller.B2SSetscore 6, BallinGame(CurrentPlayer)
	UpDateBGimage
	CheckLiSultan
	If bOnTheFirstBall(CurrentPlayer) = True And CheatStoryLevel >=1 Then LoadStoryLevel
	bModeGenieFree = False
End Sub

Sub CheckDoorLock
	If DoorON = False and (MBLock_Sav(CurrentPlayer) = 1 or MBLock_Sav(CurrentPlayer) = 3 or MBLock_Sav(CurrentPlayer) = 5) Then DoorONTimer.Enabled = True 
	If DoorON = True and (MBLock_Sav(CurrentPlayer) = 0 or MBLock_Sav(CurrentPlayer) = 2 or MBLock_Sav(CurrentPlayer) = 4) Then DoorONTimer.Enabled = True
	CheckLightMB
End Sub

Sub CheckStoryCount
	JafaScoopCount = 0
	If StoryCount(CurrentPlayer) = 0 Then Light001.State = 2 : Light002.State = 0 : Light003.State = 0 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light008.State = 2 'Capture Lamp Active
	If StoryCount(CurrentPlayer) = 1 Then Light001.State = 1 : Light002.State = 0 : Light003.State = 0 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light007.State = 0 'Capture Lamp et exit sout OK
	If StoryCount(CurrentPlayer) = 2 Then Light001.State = 1 : Light002.State = 2 : Light003.State = 0 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light007.State = 2 'Mode Tapis 1 Active
	If StoryCount(CurrentPlayer) = 3 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 0 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light007.State = 0 'Mode Tapis 1 OK
	If StoryCount(CurrentPlayer) = 4 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 2 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light007.State = 2 'Mode Tapis 2 Active
	If StoryCount(CurrentPlayer) = 5 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 0 : Light005.State = 0 : Light006.State = 0 : Light007.State = 0 'Mode Tapis 2 OK
	If StoryCount(CurrentPlayer) = 6 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 2 : Light005.State = 0 : Light006.State = 0 : Light007.State = 2 'Mode JAFA Active
	If StoryCount(CurrentPlayer) = 7 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 1 : Light005.State = 0 : Light006.State = 0 : Light007.State = 0 'Mode JAFA OK
	If StoryCount(CurrentPlayer) = 8 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 1 : Light005.State = 2 : Light006.State = 0 : Light007.State = 2 'Mode Tapis 3 Active
	If StoryCount(CurrentPlayer) = 9 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 1 : Light005.State = 1 : Light006.State = 0 : Light007.State = 0 'Mode Tapis 3 OK
	If StoryCount(CurrentPlayer) = 10 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 1 : Light005.State = 1 : Light006.State = 2 : Light007.State = 2 'Mode Serpent Active
	If StoryCount(CurrentPlayer) >= 11 Then Light001.State = 1 : Light002.State = 1 : Light003.State = 1 : Light004.State = 1 : Light005.State = 1 : Light006.State = 1 : Light007.State = 0 'Mode Serpent OK
End Sub

Sub LightStoryOFFtps
	Light001.State = 0
	Light002.State = 0 
	Light003.State = 0
	Light004.State = 0 
	Light005.State = 0 
	Light006.State = 0 
	Light007.State = 0
End Sub

Sub KickerBackL_Hit() 'kickBackLeft
	If KickBackActive(CurrentPlayer) = True or AladdinPersoState(CurrentPlayer) >= 1 Then
	'PlaySound "fx_kicker" 'burger
	playsoundat SoundFXDOF("mech_BallLauncher" , 129, DOFPulse, DOFContactors), KickerBackL 'burger
	if UseUltraDMD = 1 And AladdinPersoState(CurrentPlayer) = 0 Then UDMD "SHIELD USED", "", 3000
	Addscore 3000
	KickerBackL.kick 0, 32
	ResetLightAgrabah
	Else 
	KickerBackL.kick 90, 1
	End If
End Sub
	Playsound SoundFXDOF("fx_kicker" , 130, DOFPulse, DOFContactors)
Sub KickerBackR_Hit() 'kickBackRight
	If KickBackActive(CurrentPlayer) = True or AladdinPersoState(CurrentPlayer) >= 1 Then
'	PlaySound "fx_kicker" 'burger
	Playsoundat SoundFXDOF("mech_BallLauncher" , 112, DOFPulse, DOFContactors), KickerBackR 'burger
	if UseUltraDMD = 1 And AladdinPersoState(CurrentPlayer) = 0 Then UDMD "SHIELD USED", "", 3000
	Addscore 3000
	KickerBackR.kick 0, 32
	ResetLightAgrabah
	Else 
	KickerBackR.kick -90, 1
	End If
End Sub
Sub ResetLightAgrabah
	LeftOutlane.State = 0
	LeftInlane.State = 0
	RightInlane.State = 0
	RightOutlane.State = 0
	KickBackActive(CurrentPlayer) = False
End Sub

Sub ResetGirl
	TasmineActive = 0
	TasmineCount = 40
	RaishaActive = 0
	RaishaCount = 5
	NajwaActive = 0
	NajwaCount = 25
	If TasmineON = True Then TasmineONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	If RaishaON = True Then RaishaONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	If NajwaON = True Then NajwaONTimer.Enabled = True : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	LightTasmine001.state = 0
	LightRaisha001.state = 0
	LightNajwa001.state = 0
	LightTasmine002.state = 0
	LightRaisha002.state = 0
	LightRaisha003.state = 0
	LightRaisha004.state = 0
	LightRaisha005.state = 0
	LightRaisha006.state = 0
	LightNajwa002.state = 0
	MiRaishaTimer.Enabled = False
	MiTasmineTimer.Enabled = False
	MiNajwaTimer.Enabled = False
	PauseDMDHaremTimer.Enabled = False
	CountTimerValue1Timer.Enabled = False
	CountTimerValue2Timer.Enabled = False
	CountTimerValue3Timer.Enabled = False
	OffDMDHarem
	PauseDMDHarem = 0
End Sub 

Sub ResetSultanGirls
	ResetGirl
	EndMISultan
End Sub

Sub CheckAladdinPersoState
	If AladdinPersoState(CurrentPlayer) = 0 Then 'Mode Normal
	AladdinNormal.Visible = 1
	AladdinGuerrier.Visible = 0
	Apple001.Visible = 1
	AladdinPrince.Visible = 0
	BonusMultiplier = 1
	PlayfieldMultiplier = 1
	End If
	If AladdinPersoState(CurrentPlayer) = 1 Then 'Mode Prince bonus x2
	AladdinNormal.Visible = 0
	AladdinGuerrier.Visible = 0
	Apple001.Visible = 1
	AladdinPrince.Visible = 1
	BonusMultiplier = 2
	PlayfieldMultiplier = 1
	End If
	If AladdinPersoState(CurrentPlayer) = 2 Then 'Mode Guerrier pf X2
	AladdinNormal.Visible = 0
	AladdinGuerrier.Visible = 1
	Apple001.Visible = 0
	AladdinPrince.Visible = 0
	BonusMultiplier = 1
	PlayfieldMultiplier = 2
	End If
End Sub 
'----------Jump Jackpot and Jackpot
'JumpCount = 0
'JackpotValue(CurrentPlayer) = 0
Sub JumpTimer_Timer()
    ResetJump
End Sub

Sub ResetJump
    JumpTimer.Enabled = False
    JumpCount = 0
End Sub 

Sub CheckJump 'Up Value Jackpot Trigger013,+ 003
    JumpTimer.Enabled = False 
    JumpTimer.Interval = 6000 
    JumpCount = JumpCount + 1

    If JumpCount = 2 Then 
		JackpotValue(CurrentPlayer) = JackpotValue(CurrentPlayer) + 1500
        JumpTimer.Enabled = True 
    End If

    If JumpCount >= 3 Then 
        JackpotValue(CurrentPlayer) = JackpotValue(CurrentPlayer) + 25000
		Playsound "Fx_Bonus1"
		pDMDLabelPulseText "JackpotValue2","" & FormatNumber(JackpotValue(CurrentPlayer), 0),2000,clGold
		pDMDmessage2L "Jackpot value","+25K ",1
		pDMDLabelFadePulse "Texte2b", 2500, clRed
		if UseUltraDMD = 1 then UDMD "JUMP JACKPOT", " VALUE +25K", 3000
        ResetJump
        JumpTimer.Enabled = False
    End If
End Sub

Sub AwardJackpot 'Trigger007
	If JackpotValue(CurrentPlayer) >= 190000 Then
	AddScore2 JackpotValue(CurrentPlayer)
	DOF 450, DOFPulse
	Playsound "Fx_WinMi003"
	pDMDmessage2L "JACKPOT","" & FormatNumber(JackpotValue(CurrentPlayer), 0),1
	pDMDLabelFadePulse "Texte2a", 2500, clRed
	if UseUltraDMD = 1 then UDMD "JACKPOT", "", 3000
	JackpotValue(CurrentPlayer) = 0
	ResetJump
	End If 
End Sub
'******************
' Captive Ball Subs
'******************
Sub CapTrigger1_Hit : cbRight.TrigHit ActiveBall : End Sub
Sub CapTrigger1_UnHit : cbRight.TrigHit 0 : End Sub
Sub CapWall1_Hit : cbRight.BallHit ActiveBall : End Sub
Sub CapKicker1a_Hit : cbRight.BallReturn Me : SoundOnOrbCollision : End Sub
Sub CapKicker1_Hit : SoundOnOrbCollision : End Sub

'***********************
' Glowball Section 
'***********************
Dim GlowBall, CustomBulbIntensity(4)
Dim GBred(4)
Dim GBgreen(4)
Dim GBblue(4)
Dim CustomBallImage(4), CustomBallLogoMode(4), CustomBallDecal(4), CustomBallGlow(4)
Dim GlowAura,GlowIntensity,ChooseBall

GlowAura=230 'GlowBlob Auroa radius
GlowIntensity=25'Glowblob intensity

' blue GlowBall
CustomBallGlow(1) = 		True
GBred(1) = 1 : GBgreen(1)	= 33 : GBblue(1) = 105
' Vert GlowBall
CustomBallGlow(2) = 		True
GBred(2) = 1 : GBgreen(2)	= 33 : GBblue(2) = 5
' Rouge GlowBall
CustomBallGlow(3) = 		True
GBred(3) = 130 : GBgreen(3)	= 1 : GBblue(3) = 1
' Orange GlowBall
CustomBallGlow(4) = 		True
GBred(4) = 250 : GBgreen(4)	= 130 : GBblue(4) = 1

Dim Glowing(10)
Set Glowing(0) = Glowball1 : Set Glowing(1) = Glowball2 : Set Glowing(2) = Glowball3 : Set Glowing(3) = Glowball4 : Set Glowing(4) = Glowball4

'*** change ball appearance ***

Sub ChangeBall(ballnr)
	If ModeChangeBallActive = 0 Then
	Dim BOT, ii, col
	GlowBall = CustomBallGlow(ballnr)
	For ii = 0 to 4
		col = RGB(GBred(ballnr), GBgreen(ballnr), GBblue(ballnr))
		Glowing(ii).color = col : Glowing(ii).colorfull = col 
	Next
	End If
End Sub

' *** Ball Shadow code / Glow Ball code / Primitive Flipper Update ***

'Dim BallShadowArray
'BallShadowArray = Array (BallShadow1, BallShadow2, BallShadow3,BallShadow4,BallShadow5)


'***************************************************************
'	ZSHA: Ambient ball shadows
'***************************************************************

' For dynamic ball shadows, Check the "Raytraced ball shadows" box for the specific light. 
' Also make sure the light's z position is around 25 (mid ball)

'Ambient (Room light source)
Const AmbientBSFactor = 0.9    '0 To 1, higher is darker
Const AmbientMovement = 1	   '1+ higher means more movement as the ball moves left and right
Const offsetX = 0			   'Offset x position under ball (These are if you want to change where the "room" light is for calculating the shadow position,)
Const offsetY = 0			   'Offset y position under ball (^^for example 5,5 if the light is in the back left corner)

' *** Trim or extend these to match the number of balls/primitives/flashers on the table!  (will throw errors if there aren't enough objects)
Dim objBallShadow(10)

'Initialization
'BSInit

Sub BSInit()
	Dim iii
	'Prepare the shadow objects before play begins
	For iii = 0 To tnob - 1
		Set objBallShadow(iii) = Eval("BallShadow" & iii)
		objBallShadow(iii).material = "BallShadow" & iii
		UpdateMaterial objBallShadow(iii).material,1,0,0,0,0,0,AmbientBSFactor,RGB(0,0,0),0,0,False,True,0,0,0,0
		objBallShadow(iii).Z = 3 + iii / 1000
		objBallShadow(iii).visible = 0
	Next
End Sub


Sub BSUpdate
	BOT = Getballs
	Dim s: For s = lob To UBound(BOT)
		' *** Normal "ambient light" ball shadow
		
		'Primitive shadow on playfield, flasher shadow in ramps
		'** If on main and upper pf
		If BOT(s).Z > 20 And BOT(s).Z < 30 Then
			objBallShadow(s).visible = 1
			objBallShadow(s).X = BOT(s).X + (BOT(s).X - (tablewidth / 2)) / (Ballsize / AmbientMovement) + offsetX
			objBallShadow(s).Y = BOT(s).Y + offsetY
			'objBallShadow(s).Z = BOT(s).Z + s/1000 + 1.04 - 25	

		'** No shadow if ball is off the main playfield (this may need to be adjusted per table)
		Else
			objBallShadow(s).visible = 0
		End If
	Next
End Sub


Sub GraphicsTimer_Timer()
	Dim BOT, b
    BOT = GetBalls

	' switch off glowlight for removed Balls
	IF GlowBall Then
		For b = UBound(BOT) + 1 to 3
			If GlowBall and Glowing(b).state = 1 Then Glowing(b).state = 0 End If
		Next
	End If

    For b = 0 to UBound(BOT)
		If GlowBall and b < 4 Then
			If Glowing(b).state = 0 Then Glowing(b).state = 1 end if
			Glowing(b).BulbHaloHeight = BOT(b).z + 32
			Glowing(b).x = BOT(b).x
			Glowing(b).y = BOT(b).Y+10
			Glowing(b).falloff=GlowAura 'GlowBlob Auroa radius
			Glowing(b).intensity=GlowIntensity 'Glowblob intensity
		End If
	Next
End Sub
'************
' Rotation Light Bonus
'************
Sub RotateLaneLightsLeftUp
    Dim TempState
	TempState = RightInlane.State
    RightInlane.State = RightOutlane.State
	RightOuTlane.State = LeftOutlane.State
	LeftOutlane.State = LeftInlane.State
    LeftInlane.State = TempState
End Sub

Sub RotateLaneLightsRightUp
	Dim TempState
    TempState = LeftInlane.State
    LeftInlane.State = LeftOutlane.State
	LeftOutlane.State = RightOutlane.State
	RightOutlane.State = RightInlane.State
	RightInlane.State = TempState
End Sub


Sub UDMD (toptext, bottomtext, utime)
	If UseUltraDMD > 0 Then UltraDMD.DisplayScene00Ex "", toptext, 8, 14, bottomtext, 8,14, 14, utime, 14
End Sub
'--------- IMAGES PNG BG
Sub ResetLockBGimage
	PuPlayer.LabelSet pDMD,"Lock0","PUPAlphas\\MB000.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Lock1","PUPAlphas\\MB001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Lock2","PUPAlphas\\MB002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Lock3","PUPAlphas\\MB003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub
Sub ResetSnakeBGimage
	PuPlayer.LabelSet pDMD,"Snake000","PUPAlphas\\Snake000.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Snake001","PUPAlphas\\Snake001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Snake002","PUPAlphas\\Snake002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Snake003","PUPAlphas\\Snake003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Snake003b","PUPAlphas\\Snake003Bb.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub ResetAppleBGimage
	PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub ResetPieceBGimage
	PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub ResetJasmineBGimage
	PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
End Sub

Sub UpDateBGimage 'SNAKE + LOCK + COEURS + POMMES + PIECES
	If MBLock_Sav(CurrentPlayer) <= 1 Then 
		ResetLockBGimage
	Elseif MBLock_Sav(CurrentPlayer) = 2 or MBLock_Sav(CurrentPlayer) = 3 Then
		PuPlayer.LabelSet pDMD,"Lock0","PUPAlphas\\MB000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock1","PUPAlphas\\MB001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock2","PUPAlphas\\MB002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock3","PUPAlphas\\MB003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif MBLock_Sav(CurrentPlayer) = 4 or MBLock_Sav(CurrentPlayer) = 5  Then
		PuPlayer.LabelSet pDMD,"Lock0","PUPAlphas\\MB000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock1","PUPAlphas\\MB001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock2","PUPAlphas\\MB002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock3","PUPAlphas\\MB003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif MBLock_Sav(CurrentPlayer) >= 6 And bMultiBallMode = True Then
		PuPlayer.LabelSet pDMD,"Lock0","PUPAlphas\\MB000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock1","PUPAlphas\\MB001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock2","PUPAlphas\\MB002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Lock3","PUPAlphas\\MB003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If

	If Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 0 Then 
		ResetSnakeBGimage
	Elseif Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 1 Then
		PuPlayer.LabelSet pDMD,"Snake000","PUPAlphas\\Snake000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake001","PUPAlphas\\Snake001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake002","PUPAlphas\\Snake002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003","PUPAlphas\\Snake003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003b","PUPAlphas\\Snake003b.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"Snake000","PUPAlphas\\Snake000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake001","PUPAlphas\\Snake001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake002","PUPAlphas\\Snake002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003","PUPAlphas\\Snake003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003b","PUPAlphas\\Snake003b.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif (Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 3) And bMultiBallMode = False Then
		PuPlayer.LabelSet pDMD,"Snake000","PUPAlphas\\Snake000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake001","PUPAlphas\\Snake001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake002","PUPAlphas\\Snake002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003","PUPAlphas\\Snake003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003b","PUPAlphas\\Snake003b.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif (Snake1ok(CurrentPlayer) + Snake2ok(CurrentPlayer) + Snake3ok(CurrentPlayer) = 3) And bMultiBallMode = True Then
		PuPlayer.LabelSet pDMD,"Snake000","PUPAlphas\\Snake000.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake001","PUPAlphas\\Snake001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake002","PUPAlphas\\Snake002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003","PUPAlphas\\Snake003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Snake003b","PUPAlphas\\Snake003b.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If

	If AppleCount(CurrentPlayer) = 0 Then 
		ResetAppleBGimage
	Elseif AppleCount(CurrentPlayer) = 1 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif AppleCount(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif AppleCount(CurrentPlayer) = 3 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif AppleCount(CurrentPlayer) = 4 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif AppleCount(CurrentPlayer) = 5 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif AppleCount(CurrentPlayer) = 6 Then
		PuPlayer.LabelSet pDMD,"Pom001","PUPAlphas\\Pom001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom002","PUPAlphas\\Pom002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom003","PUPAlphas\\Pom003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom004","PUPAlphas\\Pom004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom005","PUPAlphas\\Pom005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Pom006","PUPAlphas\\Pom006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If

	If PieceCount(CurrentPlayer) = 0 Then 
		ResetPieceBGimage
	Elseif PieceCount(CurrentPlayer) = 1 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif PieceCount(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif PieceCount(CurrentPlayer) = 3 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif PieceCount(CurrentPlayer) = 4 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif PieceCount(CurrentPlayer) = 5 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif PieceCount(CurrentPlayer) = 6 Then
		PuPlayer.LabelSet pDMD,"Piece001","PUPAlphas\\Piece001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece002","PUPAlphas\\Piece002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece003","PUPAlphas\\Piece003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece004","PUPAlphas\\Piece004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece005","PUPAlphas\\Piece005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Piece006","PUPAlphas\\Piece006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If

	If JasmineCount(CurrentPlayer) = 0 Then 
		ResetJasmineBGimage
	Elseif JasmineCount(CurrentPlayer) = 1 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) = 2 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) = 3 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) = 4 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) = 5 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) = 6 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	Elseif JasmineCount(CurrentPlayer) >= 7 Then
		PuPlayer.LabelSet pDMD,"Coeur001","PUPAlphas\\Coeur001.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur002","PUPAlphas\\Coeur002.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur003","PUPAlphas\\Coeur003.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur004","PUPAlphas\\Coeur004.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur005","PUPAlphas\\Coeur005.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur006","PUPAlphas\\Coeur006.png",0,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
		PuPlayer.LabelSet pDMD,"Coeur007","PUPAlphas\\Coeur007.png",1,"{'mt':2,'width': 100, 'height': 100, 'yalign': 1}"
	End If

End Sub

'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  High Scores
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  

	' load em up


	Dim hschecker:hschecker = 0

	Sub Loadhs
		Dim x
		x = LoadValue(TableName, "HighScore1")
		If(x <> "") Then HighScore(0) = CDbl(x) Else HighScore(0) = 4000000 End If

		x = LoadValue(TableName, "HighScore1Name")
		If(x <> "") Then HighScoreName(0) = x Else HighScoreName(0) = "TOM" End If

		x = LoadValue(TableName, "HighScore2")
		If(x <> "") then HighScore(1) = CDbl(x) Else HighScore(1) = 2500000 End If

		x = LoadValue(TableName, "HighScore2Name")
		If(x <> "") then HighScoreName(1) = x Else HighScoreName(1) = "AMA" End If

		x = LoadValue(TableName, "HighScore3")
		If(x <> "") then HighScore(2) = CDbl(x) Else HighScore(2) = 1500000 End If

		x = LoadValue(TableName, "HighScore3Name")
		If(x <> "") then HighScoreName(2) = x Else HighScoreName(2) = "LIA" End If

		x = LoadValue(TableName, "HighScore4")
		If(x <> "") then HighScore(3) = CDbl(x) Else HighScore(3) = 1000000 End If

		x = LoadValue(TableName, "HighScore4Name")
		If(x <> "") then HighScoreName(3) = x Else HighScoreName(3) = "EMI" End If

		x = LoadValue(TableName, "Credits")
		If(x <> "") then Credits = CInt(x) Else Credits = 0 End If

		x = LoadValue(TableName, "TotalGamesPlayed")
		If(x <> "") then TotalGamesPlayed = CInt(x) Else TotalGamesPlayed = 0 End If

	'	If hschecker = 0 Then
	'	checkorder
	'	End If
	End Sub

	Dim hs3,hs2,hs1,hs0,hsn3,hsn2,hsn1,hsn0


	Sub checkorder
		hschecker = 1
		hs3 = HighScore(3)
		hs2 = HighScore(2)
		hs1 = HighScore(1)
		hs0 = HighScore(0)
		hsn3 = HighScoreName(3)
		hsn2 = HighScoreName(2)
		hsn1 = HighScoreName(1)
		hsn0 = HighScoreName(0)
		If hs3 > hs0 Then
			HighScore(0) = hs3
			HighScoreName(0) = hsn3	
			HighScore(1) = hs0
			HighScoreName(1) = hsn0	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2

		ElseIf hs3 > hs1 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs3
			HighScoreName(1) = hsn3	
			HighScore(2) = hs1
			HighScoreName(2) = hsn1	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 > hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs3
			HighScoreName(2) = hsn3	
			HighScore(3) = hs2
			HighScoreName(3) = hsn2
		ElseIf hs3 < hs2 Then
			HighScore(0) = hs0
			HighScoreName(0) = hsn0	
			HighScore(1) = hs1
			HighScoreName(1) = hsn1	
			HighScore(2) = hs2
			HighScoreName(2) = hsn2	
			HighScore(3) = hs3
			HighScoreName(3) = hsn3
		End If

		savehs
	End Sub


	Sub Savehs
		SaveValue TableName, "HighScore1", HighScore(0)
		SaveValue TableName, "HighScore1Name", HighScoreName(0)
		SaveValue TableName, "HighScore2", HighScore(1)
		SaveValue TableName, "HighScore2Name", HighScoreName(1)
		SaveValue TableName, "HighScore3", HighScore(2)
		SaveValue TableName, "HighScore3Name", HighScoreName(2)
		SaveValue TableName, "HighScore4", HighScore(3)
		SaveValue TableName, "HighScore4Name", HighScoreName(3)
		SaveValue TableName, "Credits", Credits
	End Sub

Sub Reseths
    HighScoreName(0) = "TOM"
    HighScoreName(1) = "AMA"
    HighScoreName(2) = "CCC"
    HighScoreName(3) = "DDD"
    HighScore(0) = 2500000
    HighScore(1) = 1500000
    HighScore(2) = 900000
    HighScore(3) = 700000
    Savehs
End Sub

	Sub Savegp
		SaveValue TableName, "TotalGamesPlayed", TotalGamesPlayed
		vpmtimer.addtimer 1000, "Loadhs'"
	End Sub


	' Initials

	Dim hsbModeActive:hsbModeActive = False
	Dim hsEnteredName
	Dim hsEnteredDigits(3)
	Dim hsCurrentDigit
	Dim hsValidLetters
	Dim hsCurrentLetter
	Dim hsLetterFlash

	' Check the scores to see if you got one

	Sub CheckHighscore()
	If halplay=0 Then
		Dim tmp
		tmp = PlayerScore(CurrentPlayer)
'		osbtempscore = Score(CurrentPlayer)

		If tmp > HighScore(0)Then 'add 1 credit for beating the highscore
			Credits = Credits + 1
'			DOF 125, DOFOn
		End If

		If tmp > HighScore(3) Then
			PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
			DOF 121, DOFPulse
			vpmtimer.addtimer 2000, "PlaySound ""vo_contratulationsgreatscore"" '"
			HighScore(3) = tmp
			'enter player's name
			HighScoreEntryInit()
		Else

			EndOfBallComplete()
		End If
	Else
		EndOfBallComplete()
		End If
	End Sub





	Sub HighScoreEntryInit()
		hsbModeActive = True
		PlaySound "vo_enteryourinitials"

		hsEnteredDigits(1) = "A"
		hsEnteredDigits(2) = " "
		hsEnteredDigits(3) = " "

		hsCurrentDigit = 1
		pDMDmessage2L "You Got a","High Score",1
		Playsound "knocker"
		HighScoreDisplayName()
		HighScorelabels	
	End Sub

	' flipper moving around the letters

	Sub EnterHighScoreKey(keycode)
		If keycode = LeftFlipperKey Then
			Playsound "fx_Previous"
				If hsletter = 0 Then
					hsletter = 26
				Else
					hsLetter = hsLetter - 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = RightFlipperKey Then
			Playsound "fx_Next"
				If hsletter = 26 Then
					hsletter = 0
				Else
					hsLetter = hsLetter + 1
				End If
				HighScoreDisplayName()
		End If

		If keycode = StartGameKey or keycode = PlungerKey Then
			PlaySound "success"
				If hsCurrentDigit = 3 Then
					If hsletter = 0 Then
						hsCurrentDigit = hsCurrentDigit -1
					Else
						assignletter
						vpmtimer.addtimer 700, "HighScoreCommitName()'"
						playclear pDMD
					End If
				End If
				If hsCurrentDigit < 3 Then
					If hsletter = 0 Then
						If hsCurrentDigit = 1 Then
						Else
							hsCurrentDigit = hsCurrentDigit -1
						End If
					Else
						assignletter
						hsCurrentDigit = hsCurrentDigit + 1
						HighScoreDisplayName()

					End If
				End If
		End if
	End Sub

	Dim hsletter
	hsletter = 1

	dim hsdigit:hsdigit = 1

	Sub assignletter
		if hscurrentdigit = 1 Then
			hsdigit = 1
		End If
		if hscurrentdigit = 2 Then
			hsdigit = 2
		End If
		if hscurrentdigit = 3 Then
			hsdigit = 3
		End If
		If hsletter = 1 Then 
			hsEnteredDigits(hsdigit) = "A"
		End If
		If hsletter = 2 Then 
			hsEnteredDigits(hsdigit) = "B"
		End If
		If hsletter = 3 Then 
			hsEnteredDigits(hsdigit) = "C"
		End If
		If hsletter = 4 Then 
			hsEnteredDigits(hsdigit) = "D"
		End If
		If hsletter = 5 Then 
			hsEnteredDigits(hsdigit) = "E"
		End If
		If hsletter = 6 Then 
			hsEnteredDigits(hsdigit) = "F"
		End If
		If hsletter = 7 Then 
			hsEnteredDigits(hsdigit) = "G"
		End If
		If hsletter = 8 Then 
			hsEnteredDigits(hsdigit) = "H"
		End If
		If hsletter = 9 Then 
			hsEnteredDigits(hsdigit) = "I"
		End If
		If hsletter = 10 Then 
			hsEnteredDigits(hsdigit) = "J"
		End If
		If hsletter = 11 Then 
			hsEnteredDigits(hsdigit) = "K"
		End If
		If hsletter = 12 Then 
			hsEnteredDigits(hsdigit) = "L"
		End If
		If hsletter = 13 Then 
			hsEnteredDigits(hsdigit) = "M"
		End If
		If hsletter = 14 Then 
			hsEnteredDigits(hsdigit) = "N"
		End If
		If hsletter = 15 Then 
			hsEnteredDigits(hsdigit) = "O"
		End If
		If hsletter = 16 Then 
			hsEnteredDigits(hsdigit) = "P"
		End If
		If hsletter = 17 Then 
			hsEnteredDigits(hsdigit) = "Q"
		End If
		If hsletter = 18 Then 
			hsEnteredDigits(hsdigit) = "R"
		End If
		If hsletter = 19 Then 
			hsEnteredDigits(hsdigit) = "S"
		End If
		If hsletter = 20 Then 
			hsEnteredDigits(hsdigit) = "T"
		End If
		If hsletter = 21 Then 
			hsEnteredDigits(hsdigit) = "U"
		End If
		If hsletter = 22 Then 
			hsEnteredDigits(hsdigit) = "V"
		End If
		If hsletter = 23 Then 
			hsEnteredDigits(hsdigit) = "W"
		End If
		If hsletter = 24 Then 
			hsEnteredDigits(hsdigit) = "X"
		End If
		If hsletter = 25 Then 
			hsEnteredDigits(hsdigit) = "Y"
		End If
		If hsletter = 26 Then 
			hsEnteredDigits(hsdigit) = "Z"
		End If

	End Sub

	Sub HighScorelabels
		PuPlayer.LabelSet pDMD,"HighScore","Enter Your Name :",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL2","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL3","",1,""
'		PuPlayer.LabelSet pDMD,"HighScoreL4",PlayerScore(CurrentPlayer),1,""
		hsletter = 1
	End Sub

	Sub HighScoreDisplayName()

		Select case hsLetter
		Case 0
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","<",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","<",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","<",1,""
		Case 1
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","A",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","A",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","A",1,""
		Case 2
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","B",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","B",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","B",1,""
		Case 3
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","C",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","C",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","C",1,""
		Case 4
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","D",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","D",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","D",1,""
		Case 5
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","E",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","E",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","E",1,""
		Case 6
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","F",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","F",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","F",1,""
		Case 7
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","G",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","G",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","G",1,""
		Case 8
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","H",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","H",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","H",1,""
		Case 9
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","I",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","I",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","I",1,""
		Case 10
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","J",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","J",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","J",1,""
		Case 11
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","K",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","K",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","K",1,""
		Case 12
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","L",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","L",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","L",1,""
		Case 13
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","M",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","M",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","M",1,""
		Case 14
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","N",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","N",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","N",1,""
		Case 15
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","O",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","O",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","O",1,""
		Case 16
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","P",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","P",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","P",1,""
		Case 17
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Q",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Q",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Q",1,""
		Case 18
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","R",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","R",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","R",1,""
		Case 19
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","S",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","S",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","S",1,""
		Case 20
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","T",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","T",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","T",1,""
		Case 21
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","U",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","U",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","U",1,""
		Case 22
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","V",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","V",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","V",1,""
		Case 23
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","W",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","W",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","W",1,""
		Case 24
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","X",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","X",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","X",1,""
		Case 25
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Y",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Y",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Y",1,""
		Case 26
			if(hsCurrentDigit = 1) then PuPlayer.LabelSet pDMD,"HighScoreL1","Z",1,""
			if(hsCurrentDigit = 2) then PuPlayer.LabelSet pDMD,"HighScoreL2","Z",1,""
			if(hsCurrentDigit = 3) then PuPlayer.LabelSet pDMD,"HighScoreL3","Z",1,""
		End Select
	End Sub

	' post the high score letters


	Sub HighScoreCommitName()
		playclear pDMD
		PuPlayer.SetLoop 2,0
		PuPlayer.SetLoop 7,0
		hsEnteredName = hsEnteredDigits(1) & hsEnteredDigits(2) & hsEnteredDigits(3)
		HighScoreName(3) = hsEnteredName
		checkorder
		osbtemp = hsEnteredName
		if osbkey="" Then
		Else
		SubmitOSBScore
		end If
		EndOfBallComplete()
		PuPlayer.LabelSet pDMD,"HighScore","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL1","",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL2"," ",1,""
		PuPlayer.LabelSet pDMD,"HighScoreL3"," ",1,""
'		PuPlayer.LabelSet pDMD,"HighScoreL4"," ",1,""
		hsbModeActive = False
	End Sub

sub playclear(chan)
		debug.print "play clear'd " & chan

		if chan = pMusic Then
			PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 4, ""FN"":11, ""VL"":0 }"
		End If

		if chan = pBackglass Then
			PuPlayer.playstop pDMD
		End If
	end Sub


'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'  ATTRACT MODE
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
'/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/ \/
'\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\ /\
' X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  X  
' 

	Sub StartAttractMode()
		DOF 323, DOFOn   'DOF MX - Attract Mode ON
		bAttractMode = True
		ChangeSong
		StartLightSeq
	End Sub

	Sub StopAttractMode()
		pDMDStartGame
		DOF 323, DOFOff   'DOF MX - Attract Mode Off
		bAttractMode = False
		LightSeqAttract.StopPlay		
	'StopSong
	End Sub

'*****************
'  VR 
'*****************

DIM VRThings
DIM VRRoom

Sub LoadVRRoom
	for each VRThings in VR_Cab:VRThings.visible = 0:Next
	for each VRThings in VR_Min:VRThings.visible = 0:Next
	for each VRThings in VR_Mega:VRThings.visible = 0:Next
'	VR_Billboards_D4_Billboards_D4_.StopAnim
	VR_Fog_far_Fog_0_001.StopAnim
	VR_Fog_near_Fog_0_001.StopAnim

	If RenderingMode = 2 or VRTest Then
		VRRoom = VRRoomChoice
		'disable table objects that should not be visible
		ramp17.visible = 0
		Wall001.sidevisible = 0
		Wall002.sidevisible = 0
		ramp16.visible = 0
		ramp15.visible = 0
	Else
		VRRoom = 0
	End If
	If VRRoom = 1 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 2 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 1:Next
		for each VRThings in VR_Mega:VRThings.visible = 0:Next
	End If
	If VRRoom = 3 Then
		for each VRThings in VR_Cab:VRThings.visible = 1:Next
		for each VRThings in VR_Min:VRThings.visible = 0:Next
		for each VRThings in VR_Mega:VRThings.visible = 1:Next
'		VR_Billboards_D4_Billboards_D4_.PlayAnimEndless(0.4)
		VR_Fog_far_Fog_0_001.PlayAnimEndless(0.05)
		VR_Fog_near_Fog_0_001.PlayAnimEndless(0.05)
	End If
End Sub

Sub LoadStoryLevel
	If CheatStoryLevel = 1 Then
		LampSouterrain(CurrentPlayer) = 0
		LampSoutCollected(CurrentPlayer) = 0
		MysteryState(CurrentPlayer) = 1
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 0
		CheckStoryCount
	End If 
	If CheatStoryLevel = 2 Then 'lamp ok
		LampSouterrain(CurrentPlayer) = 11
		LampSoutCollected(CurrentPlayer) = 2
		ResetMystery
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 1
		CheckStoryCount
	End If
	If CheatStoryLevel = 3 Then 'Tapi red ok
		LampSouterrain(CurrentPlayer) = 11
		LampSoutCollected(CurrentPlayer) = 2
		ResetMystery
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 3
		CheckStoryCount
	End If
	If CheatStoryLevel = 4 Then 'tapi blue ok
		LampSouterrain(CurrentPlayer) = 11
		LampSoutCollected(CurrentPlayer) = 2
		ResetMystery
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 5
		CheckStoryCount
	End If
	If CheatStoryLevel = 5 Then 'Boss2 ok
		LampSouterrain(CurrentPlayer) = 11
		LampSoutCollected(CurrentPlayer) = 2
		ResetMystery
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 7
		CheckStoryCount
	End If
	If CheatStoryLevel = 6 Then 'Sabre ok
		LampSouterrain(CurrentPlayer) = 11
		LampSoutCollected(CurrentPlayer) = 2
		ResetMystery
		CheckModeCaverne
		StoryCount(CurrentPlayer) = 10
		CheckStoryCount
	End If
End Sub
'***********Souterrain

Sub CheckPositionAladdin
	If LampSouterrain(CurrentPlayer) = 0 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 0 : Aladdin001.TransZ = 0 : LampPr001.visible = 1
	If LampSouterrain(CurrentPlayer) = 1 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 0 : Aladdin001.TransZ = -70 : LampPr001.visible = 1
	If LampSouterrain(CurrentPlayer) = 2 Then Aladdin001.RotY = 90 : Aladdin001.TransX = -160 : Aladdin001.TransZ = 0 : LampPr001.visible = 1
	If LampSouterrain(CurrentPlayer) = 3 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 65 : Aladdin001.TransZ = -160 : LampPr001.visible = 1 : Pupevent 842
	If LampSouterrain(CurrentPlayer) = 4 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 65 : Aladdin001.TransZ = -230 : LampPr001.visible = 1
	If LampSouterrain(CurrentPlayer) = 5 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 65 : Aladdin001.TransZ = -290 : LampPr001.visible = 1
	If LampSouterrain(CurrentPlayer) = 6 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 65 : Aladdin001.TransZ = -350 : LampPr001.visible = 1 : Pupevent 843
	If LampSouterrain(CurrentPlayer) = 7 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 65 : Aladdin001.TransZ = -410
	If LampSouterrain(CurrentPlayer) = 8 And LampSoutCollected(CurrentPlayer) = 0 Then Aladdin001.RotY = 90 : Aladdin001.TransX = -410 : Aladdin001.TransZ = -150 : LampPr001.visible = 0 : LampSoutCollected(CurrentPlayer) = 1 : AddScore2 45000 : PlaySound "Fx_LampCollect" : Pupevent 844 : ChangeBall(4) : ChangePalmYel 'jaune
	If LampSouterrain(CurrentPlayer) = 8 And LampSoutCollected(CurrentPlayer) = 1 Then Aladdin001.RotY = 90 : Aladdin001.TransX = -410 : Aladdin001.TransZ = -150 : LampPr001.visible = 0 : LampSoutCollected(CurrentPlayer) = 1
	If LampSouterrain(CurrentPlayer) = 9 Then Aladdin001.RotY = -90 : Aladdin001.TransX = 410 : Aladdin001.TransZ = 0
	If LampSouterrain(CurrentPlayer) = 10 Then Aladdin001.RotY = 180 : Aladdin001.TransX = 0 : Aladdin001.TransZ = -460
	If LampSouterrain(CurrentPlayer) = 11 And LampSoutCollected(CurrentPlayer) = 0 Then StopModeSouterrain : AddScore2 75000 : Pupevent 845
	If LampSouterrain(CurrentPlayer) = 11 And LampSoutCollected(CurrentPlayer) = 1 Then 
		StoryCount(CurrentPlayer) = 1 
		LampSoutCollected(CurrentPlayer) = 2
		ChangeBall(0) 'off
		ChangePalmBICOL
		EnableBallSaver 15
		StopModeSouterrain
		FlashForMs Flasher004, 500, 50, 0
		FlashForMs F1A009, 500, 50, 0
		Pupevent 847
		If usePuPDMD = False Then PlaySound "Vo_EndCav" 
	End If
End Sub

Sub Target008_Hit() 'L
	Playsound "Target_Hit_3"
	If LampSouterrain(CurrentPlayer) = 2 Or LampSouterrain(CurrentPlayer) = 7 Then
	AddScore 900
	LampSouterrain(CurrentPlayer) = LampSouterrain(CurrentPlayer) + 1
	CheckPositionAladdin
	End If
	If LampSouterrain(CurrentPlayer) = 9 Then
	AddScore 900
	LampSouterrain(CurrentPlayer) = LampSouterrain(CurrentPlayer) - 2
	CheckPositionAladdin
	End If
End Sub

Sub Target009_Hit() 'R
	Playsound "Target_Hit_3"
	If LampSouterrain(CurrentPlayer) = 7 Or LampSouterrain(CurrentPlayer) = 8 Then
	AddScore 900
	LampSouterrain(CurrentPlayer) = LampSouterrain(CurrentPlayer) + 1
	CheckPositionAladdin
	End If
	If LampSouterrain(CurrentPlayer) = 3 Then
	AddScore 900
	LampSouterrain(CurrentPlayer) = LampSouterrain(CurrentPlayer) - 1
	CheckPositionAladdin
	End If
End Sub

Sub Target010_Hit() 'UP
	Playsound "Target_Hit_3"
	If LampSouterrain(CurrentPlayer) = 0 Or LampSouterrain(CurrentPlayer) = 1 Or LampSouterrain(CurrentPlayer) = 3 Or LampSouterrain(CurrentPlayer) = 4 Or LampSouterrain(CurrentPlayer) = 5 Or LampSouterrain(CurrentPlayer) = 6 Or LampSouterrain(CurrentPlayer) = 9 Or LampSouterrain(CurrentPlayer) = 10 Then
	AddScore 900
	LampSouterrain(CurrentPlayer) = LampSouterrain(CurrentPlayer) + 1
	CheckPositionAladdin
	End If
End Sub

Sub Trigger001_Hit()
	StopModeSouterrain
	EndMegaTapi
	EndModeSnake
	Trigger001.destroyball
	FlasherPFA.Visible = True
	GiSoutOFF
	GiOn
	Kicker004.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker004.timerinterval = 4000
	kicker004.timerenabled = True
End Sub


Sub CheckModeCaverne
	If LampSoutCollected(CurrentPlayer) = 2 Then
	LampPr002.Visible = 1
	Cave.Visible = 0
	Cave001.Visible = 0
	Tigre.Visible = 1
	JasmineBody.Visible = 1
	JasmineFace.Visible = 1
	JasmineTop.Visible = 1
	JasmineShort.Visible = 1
	JasmineHair.Visible = 1
	JasmineBijoux.Visible = 1
	JasmineBijoux2.Visible = 1
	RockA.Visible = 0
	RockB.Visible = 0
	ModeSpinnersActive(CurrentPlayer) = True
	Light010.state = 2
	Else
	LampPr002.Visible = 0
	Genie.Visible = 0
	Cave.Visible = 1
	Cave001.Visible = 1
	Tigre.Visible = 0
	JasmineBody.Visible = 0
	JasmineFace.Visible = 0
	JasmineTop.Visible = 0
	JasmineShort.Visible = 0
	JasmineHair.Visible = 0
	JasmineBijoux.Visible = 0
	JasmineBijoux2.Visible = 0
	ModeSpinnersActive(CurrentPlayer) = False
	StoryCount(CurrentPlayer) = 0
	Light010.state = 0
	End If
End Sub	

Sub StopModeSouterrain
	RockA.Visible = 0
	RockB.Visible = 0
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 1 : GlowBatLightRight.State = 1
	LiUP.State=0
	LiRight.State=0
	LiLeft.State=0
	bModeSouterrainActive = False
	PauseDMDHaremTimer.Enabled=True
	CheckModeCaverne
	CheckStoryCount
End Sub

Sub StartModeSouterrain
	If LampSouterrain(CurrentPlayer) >= 6 And LampSoutCollected(CurrentPlayer) = 0 Then LampSouterrain(CurrentPlayer) = 4
	CheckPositionAladdin		
	EndMegaTapi
	kicker002.destroyball
	RockA.Visible = 1
	RockB.Visible = 1
	FlasherPFA.Visible = False
	GiSoutON
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 0 : GlowBatLightRight.State = 0
	GiOff
	LiUP.State=2
	LiRight.State=2
	LiLeft.State=2
	bModeSouterrainActive = True
	LightStoryOFFtps
	Kicker003.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker003.timerinterval = 4000
	kicker003.timerenabled = True
End Sub

Sub StartModeSnake
	If usePuPDMD = False Then Playsound "Fx_EnterSnake" Else PuPEvent 851
	RockA.Visible = 0
	RockB.Visible = 0
	SnakeDB.Visible = 1
	SnakeDA.Visible = 1
	FlasherPFA.Visible = False
	FlasherPFB.ImageA = "RampPFb8"
	GiSoutON
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 0 : GlowBatLightRight.State = 0
	GiOff
	LightStoryOFFtps
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	Kicker003.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker003.timerinterval = 3000
	kicker003.timerenabled = True
	MegaTapis.Visible = 0
	MegaTapis001.Visible = 0
	MegaTapis.Collidable = 0
	MegaTapis002.Collidable = 0
	Target008.Visible = 0
	Target008.Collidable = 0
	Target009.Visible = 0
	Target009.Collidable = 0
	Target010.Visible = 0
	Target010.Collidable = 0
	bModeSouterrainActive = True
	RFLogo001.Visible = 1
	RightFlipper001.Enabled = 1
	Aladdin001.Visible = 0
	LampPr001.Visible = 0
	Wall015.Collidable = 0
	Wall037.Collidable = 0
	Wall051.Collidable = 1
	Wall052.Collidable = 1
	Gate006.Collidable = 0
	Gate007.Collidable = 1
	LiSnake001.State = 1
	LiSnake002.State = 1
	LiSnake003.State = 1
	LiSnake004.State = 1
	LiSnake005.State = 1
	LiLifeBoss001.state = 0
	LiLifeBoss002.state = 0
	LiLifeBoss003.state = 0
	LiLifeBoss004.state = 0
	LiLifeBoss005.state = 0
	LiLifeAladd001.state = 0
	LiLifeAladd002.state = 0
	LiLifeAladd003.state = 0
	Kicker007.Enabled = 1
	Kicker006.Enabled = 0
	Trigger011.Enabled = 1
	Ramp024.Collidable = 0
	PegS003.Collidable = 1
	TargetSnak001.Collidable = 1
	TargetSnak002.Collidable = 1
	TargetSnak003.Collidable = 1
	TargetSnak004.Collidable = 1
	TargetSnak005.Collidable = 1
End Sub
Sub EndModeSnake
	SnakeHeadBossTimer.enabled=False
	PauseDMDHaremTimer.Enabled=True
	bModeSouterrainActive = False
	If StoryCount(CurrentPlayer) = 11 Then
		AddMB1
		EnableBallSaver 15
		If usePuPDMD = False Then Playsound "Fx_WinMi002" Else Playsound "Fx_WinMi002" : PupEvent 855 
	End If
	SnakeDB.Visible = 0
	SnakeDA.Visible = 0
	Wall015.Collidable = 1
	Wall037.Collidable = 1
	Wall051.Collidable = 0
	Wall052.Collidable = 0
	Gate006.Collidable = 1
	Gate007.Collidable = 0
	LiSnake001.State = 0
	LiSnake002.State = 0
	LiSnake003.State = 0
	LiSnake004.State = 0
	LiSnake005.State = 0
	Kicker007.Enabled = 0
	Kicker006.Enabled = 1
	Trigger011.Enabled = 0
	Ramp024.Collidable = 1
	PegS003.Collidable = 0
	TargetSnak001.Collidable = 0
	TargetSnak002.Collidable = 0
	TargetSnak003.Collidable = 0
	TargetSnak004.Collidable = 0
	TargetSnak005.Collidable = 0
End Sub

Sub Trigger011_Hit()
	ChangeBall(0)
	AddScore 155
End Sub

' SHAKE/MOOVE SnakeBoss
'**********************************
Dim SnakeBossPos

Sub ShakeSnakeBoss
    SnakeBossPos = 8
    SnakeBossTimer.Enabled = 1
End Sub
Sub SnakeBossTimer_Timer
	SnakeDB.TransY = SnakeBossPos
	SnakeDA.TransY = SnakeBossPos
    If SnakeBossPos = 0 Then Me.Enabled = 0:Exit Sub
    If SnakeBossPos < 0 Then
        SnakeBossPos = ABS(SnakeBossPos) - 1
    Else
        SnakeBossPos = - SnakeBossPos + 1
    End If
End Sub

Dim SnakeHeadBossON
SnakeHeadBossON = False

Sub SnakeHeadBossTimer_Timer()
If not SnakeHeadBossON Then
	SnakeDB.RotY = SnakeDB.RotY -5
	SnakeDB.RotX = SnakeDB.RotX -0.5
	If SnakeDB.RotY = -40 Then
		SnakeHeadBossTimer.enabled=True
		SnakeHeadBossON = True
	End IF
Else	
	If SnakeDB.RotY = 0 Then
		SnakeHeadBossTimer.enabled=False
		SnakeHeadBossON = False
	Else
	SnakeDB.RotY = SnakeDB.RotY +5
	SnakeDB.RotX = SnakeDB.RotX +0.5
	End IF
End If
End Sub

Sub TargetSnak001_Hit()
	AddScore 6500
	If LiSnake001.State = 1 Then 
	LiSnake001.State = 2
	Playsound "fx_snakeHit"
	Elseif LiSnake001.State = 2 Then
	LiSnake001.State = 0
	DOF 119, DOFPulse 'shaker
	DOF 448, DOFPulse 'MX Explosion
	If usePuPDMD = False Then Playsound "fx_snakeHit" Else PupEvent 852 
	FlasherPFB.ImageA = "RampPFb8"
	ShakeSnakeBoss
	CheckAwardSnake
	End If
End Sub
Sub TargetSnak002_Hit()
	AddScore 6500
	If LiSnake002.State = 1 Then 
	LiSnake002.State = 2
	Playsound "fx_snakeHit"
	Elseif LiSnake002.State = 2 Then
	LiSnake002.State = 0
	DOF 119, DOFPulse 'shaker
	DOF 448, DOFPulse 'MX Explosion
	Playsound "fx_snakeHit"
	FlasherPFB.ImageA = "RampPFb9"
	ShakeSnakeBoss
	CheckAwardSnake
	End If
End Sub
Sub TargetSnak003_Hit()
	AddScore 6500
	If LiSnake003.State = 1 Then 
	LiSnake003.State = 2
	Playsound "fx_snakeHit"
	Elseif LiSnake003.State = 2 Then
	LiSnake003.State = 0
	DOF 119, DOFPulse 'shaker
	DOF 448, DOFPulse 'MX Explosion
	If usePuPDMD = False Then Playsound "fx_snakeHit" Else PupEvent 853 
	FlasherPFB.ImageA = "RampPFb9"
	ShakeSnakeBoss
	CheckAwardSnake
	End If
End Sub
Sub TargetSnak004_Hit()
	AddScore 6500
	If LiSnake004.State = 1 Then 
	LiSnake004.State = 2
	Playsound "fx_snakeHit"
	Elseif LiSnake004.State = 2 Then
	LiSnake004.State = 0
	DOF 119, DOFPulse 'shaker
	DOF 448, DOFPulse 'MX Explosion
	Playsound "fx_snakeHit"
	FlasherPFB.ImageA = "RampPFb9"
	ShakeSnakeBoss
	CheckAwardSnake
	End If
End Sub
Sub TargetSnak005_Hit()
	AddScore 6500
	If LiSnake005.State = 1 Then 
	LiSnake005.State = 2
	Playsound "fx_snakeHit"
	Elseif LiSnake005.State = 2 Then
	LiSnake005.State = 0
	DOF 119, DOFPulse 'shaker
	DOF 448, DOFPulse 'MX Explosion
	If usePuPDMD = False Then Playsound "fx_snakeHit" Else PupEvent 854 
	FlasherPFB.ImageA = "RampPFb8"
	ShakeSnakeBoss
	CheckAwardSnake
	End If
End Sub

Sub CheckAwardSnake
	If LiSnake001.State = 0 and LiSnake002.State = 0 and LiSnake003.State = 0 and LiSnake004.State = 0 and LiSnake005.State = 0 Then
	AddScore2 650000 
	if UseUltraDMD = 1 then UDMD "JAFA DEAD", "650K", 3000
	pDMDmessage2L "Jafa Dead","+ 650 K",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	StoryCount(CurrentPlayer) = 11
	bModeSouterrainActive = False
	End If
End Sub

sub kicker007_hit()
	AddScore 550
	If bModeSouterrainActive = True Then kicker007.kick -25, 40 : ChangeBall(1) : ChangePalmBlue :  PlaySoundJump : DOF 119, DOFPulse 'shaker
	If bModeSouterrainActive = False Then kicker007.kick 25, 30
	SnakeHeadBossTimer.enabled=True
End Sub

'sub kicker008_hit() 'Débug ramp Sultant
'	kicker008.kick 25, 15
'End Sub

Sub Kicker003_Timer()
	'PlaySound "fx_kicker" 'burger
	playsoundat "warehousekick", Kicker003 'burger
        DOF 124, DOFPulse
	kicker003.kick 45, 5'60
	kicker003.timerenabled = False
End Sub

Sub Kicker006_Timer()	
	If ModeMegaTapisActive = True Then 
		Playsound "Fx_MegaTapis"
		PupEvent 850
	End If
	'PlaySound "fx_kicker" 'burger
	playsoundat "warehousekick", Kicker006 'burger
	kicker006.kick 190, 2'
    DOF 113, DOFPulse
	kicker006.timerenabled = False
End Sub

Sub Kicker008_Hit()	
	kicker008.kick 25, 20
End Sub

'************Pieces
Sub AladpieceRottimer_Timer()
	AladdPiece.visible = 1
	AladdPiece.ObjRotZ = AladdPiece.ObjRotZ +4
	Sabre001.ObjRotZ = Sabre001.ObjRotZ +4
	Diam001.ObjRotZ = Diam001.ObjRotZ +4
	Diam002.ObjRotZ = Diam002.ObjRotZ +4
	discolight001.RotZ = discolight001.RotZ +4
	discolight002.RotZ = discolight002.RotZ +4
End Sub
'************ Moove BrasSinge
Dim MonkerRotON
MonkerRotON = False

Sub MonkeyRTimer_Timer()
If not MonkerRotON Then
	MonkAbuBras.ObjRotZ = MonkAbuBras.ObjRotZ +1
	MonkAbuBras.RotX = MonkAbuBras.RotX -4.5
	MonkAppleR001.ObjRotZ = MonkAppleR001.ObjRotZ +1
	MonkAppleR001.RotX = MonkAppleR001.RotX -4.5
	If MonkAbuBras.ObjRotZ = 40 Then
		MonkeyRTimer.enabled=True
		MonkerRotON=True
	End IF
Else	
	If MonkAbuBras.ObjRotZ = 0 Then
		MonkeyRTimer.enabled=False
		MonkerRotON=False
	Else
	MonkAbuBras.ObjRotZ = MonkAbuBras.ObjRotZ -2
	MonkAbuBras.RotX = MonkAbuBras.RotX +9
	MonkAppleR001.ObjRotZ = MonkAppleR001.ObjRotZ -2
	MonkAppleR001.RotX = MonkAppleR001.RotX +9
		If MonkAbuBras.ObjRotZ = 20 Then
'			Apple1Timer.enabled=True
			MonkAppleR001.visible = 0
		End IF
	End IF
End If
End Sub

'************ Moove iago
Dim IagoON
IagoON = False

Sub IagoTimer_Timer()
If not IagoON Then
	IAGOR.ObjRotX = IAGOR.ObjRotX -0.25
	IAGOR.ObjRotY = IAGOR.ObjRotY +1
	IAGOR.ObjRotZ = IAGOR.ObjRotZ +0.25
	IAGOL.ObjRotX = IAGOL.ObjRotX +0.25
	IAGOL.ObjRotY = IAGOL.ObjRotY -1
	IAGOL.ObjRotZ = IAGOL.ObjRotZ -0.25
	IAGO.TransY = IAGO.TransY +0.5
	IAGOR.TransY = IAGOR.TransY +0.5
	IAGOL.TransY = IAGOL.TransY +0.5
	If IAGOR.ObjRotY = 40 Then
		IagoTimer.enabled=True
		IagoON=True
	End IF
Else	
	If IAGOR.ObjRotY = 0 Then
		IagoTimer.enabled=False
		IagoON=False
	Else
	IAGOR.ObjRotX = IAGOR.ObjRotX +0.25
	IAGOR.ObjRotY = IAGOR.ObjRotY -1
	IAGOR.ObjRotZ = IAGOR.ObjRotZ -0.25
	IAGOL.ObjRotX = IAGOL.ObjRotX -0.25
	IAGOL.ObjRotY = IAGOL.ObjRotY +1
	IAGOL.ObjRotZ = IAGOL.ObjRotZ +0.25
	IAGO.TransY = IAGO.TransY -0.5
	IAGOR.TransY = IAGOR.TransY -0.5
	IAGOL.TransY = IAGOL.TransY -0.5
	End IF
End If
End Sub
'************ Moove JAFA
Dim JafaSnakeON, JafaCannON
JafaSnakeON = False
JafaCannON = False

Sub JafaSnakeTimer_Timer()
If not JafaSnakeON Then
	JafaSnake.ObjRotZ = JafaSnake.ObjRotZ +0.5
	JafaSnake.TransZ = JafaSnake.TransZ +0.5
	If JafaSnake.TransZ = 10 Then
		JafaSnakeTimer.enabled=True
		JafaSnakeON=True
	End IF
Else	
	If JafaSnake.TransZ = 0 Then
		JafaSnakeTimer.enabled=False
		JafaSnakeON=False
	Else
	JafaSnake.ObjRotZ = JafaSnake.ObjRotZ -0.5
	JafaSnake.TransZ = JafaSnake.TransZ -0.5
	End IF
End If
End Sub

Sub JafaCannTimer_Timer()
If not JafaCannON Then
	JafaEyes.Visible = 1
	JafaCannEyes.Visible = 1
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	JafaCann.ObjRotZ = JafaCann.ObjRotZ +1
	JafaCann.TransZ = JafaCann.TransZ +1
	JafaCannEyes.ObjRotZ = JafaCannEyes.ObjRotZ +1
	JafaCannEyes.TransZ = JafaCannEyes.TransZ +1
	If JafaCann.TransZ = 15 Then
		If AnimTrailerTimer.Enabled = False Then 
			PlaySoundThunder
			DOF 127, DOFPulse 'blower
			DOF 449, DOFPulse 'mx left Electro
		End If 
		FlashForMs Flasher004, 1000, 300, 0
		FlashForMs F1A002, 1000, 300, 0
		FlashForMs F1A009, 1000, 300, 0
		JafaCannTimer.enabled=True
		JafaCannON=True
	End IF
Else	
	If JafaCann.TransZ = 0 Then
		JafaCannTimer.enabled=False
		JafaEyes.Visible = 0
		JafaCannEyes.Visible = 0
		FlashBotelL.Visible = 0
		FlashBotelR.Visible = 0
		F1A008.state = 0
		F1A005.state = 0
		JafaCannON=False
	Else
	JafaCann.ObjRotZ = JafaCann.ObjRotZ -0.5
	JafaCann.TransZ = JafaCann.TransZ -0.5
	JafaCannEyes.ObjRotZ = JafaCannEyes.ObjRotZ -0.5
	JafaCannEyes.TransZ = JafaCannEyes.TransZ -0.5
	End IF
End If
End Sub
'************ Moove AppleS
Sub StartAppel1
	If AppleCount(CurrentPlayer) >= 1 Then
	MonkeyRTimer.enabled=True
	vpmtimer.addtimer 600, "Apple1Timer.enabled=True '"
	End If
End Sub

Sub StartAppel2
	If AppleCount(CurrentPlayer) >= 1 Then
	MonkeyRTimer.enabled=True
	vpmtimer.addtimer 600, "Apple2Timer.enabled=True '"
	End If
End Sub

Sub StartAppel3
	If AppleCount(CurrentPlayer) >= 1 Then
	MonkeyRTimer.enabled=True
	vpmtimer.addtimer 600, "Apple3Timer.enabled=True '"
	End If
End Sub

Sub Apple1Timer_Timer()
	MonkApple001.visible = 1
	MonkApple001.TransY = MonkApple001.TransY +1
	MonkApple001.TransX = MonkApple001.TransX -7.25
	MonkApple001.TransZ = MonkApple001.TransZ -26
	If MonkApple001.TransY = 20 Then
		Apple1Timer.enabled=False
		AppleCount(CurrentPlayer) = AppleCount(CurrentPlayer) - 1
		MonkApple001.visible = 0
		Snake001.visible = 0
		MonkApple001.TransY = 0
		MonkApple001.TransX = 0
		MonkApple001.TransZ = 0
		CheckAppleCount
		CheckSnakeOK
	End IF
End Sub
Sub Apple2Timer_Timer()
	MonkApple002.visible = 1
	MonkApple002.TransY = MonkApple002.TransY +1
	MonkApple002.TransX = MonkApple002.TransX -13.5
	MonkApple002.TransZ = MonkApple002.TransZ -28
	If MonkApple002.TransY = 20 Then
		Apple2Timer.enabled=False
		AppleCount(CurrentPlayer) = AppleCount(CurrentPlayer) - 1
		MonkApple002.visible = 0
		Snake002.visible = 0
		MonkApple002.TransY = 0
		MonkApple002.TransX = 0
		MonkApple002.TransZ = 0
		CheckAppleCount
		CheckSnakeOK
	End IF
End Sub
Sub Apple3Timer_Timer()
	MonkApple003.visible = 1
	MonkApple003.TransY = MonkApple003.TransY +1
	MonkApple003.TransX = MonkApple003.TransX +12.75
	MonkApple003.TransZ = MonkApple003.TransZ -21.25
	If MonkApple003.TransY = 20 Then
		Apple3Timer.enabled=False
		AppleCount(CurrentPlayer) = AppleCount(CurrentPlayer) - 1
		MonkApple003.visible = 0
		Snake003.visible = 0
		MonkApple003.TransY = 0
		MonkApple003.TransX = 0
		MonkApple003.TransZ = 0
		CheckAppleCount
		CheckSnakeOK
	End IF
End Sub

Sub CheckAppleCount
	If AppleCount(CurrentPlayer) <= 0 Then AppleCount(CurrentPlayer) = 0 : MonkAppleR001.visible = 0 : LightApple001.state = 0 : LightApple002.state = 0 : LightApple003.state = 0 : LightApple004.state = 0 : LightApple005.state = 0 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 1 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 0 : LightApple003.state = 0 : LightApple004.state = 0 : LightApple005.state = 0 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 2 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 0 : LightApple004.state = 0 : LightApple005.state = 0 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 3 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 1 : LightApple004.state = 0 : LightApple005.state = 0 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 4 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 1 : LightApple004.state = 1 : LightApple005.state = 0 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 5 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 1 : LightApple004.state = 1 : LightApple005.state = 1 : LightApple006.state = 0
	If AppleCount(CurrentPlayer) = 6 Then MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 1 : LightApple004.state = 1 : LightApple005.state = 1 : LightApple006.state = 1
	If AppleCount(CurrentPlayer) >= 7 Then AppleCount(CurrentPlayer) = 6 : MonkAppleR001.visible = 1 : LightApple001.state = 1 : LightApple002.state = 1 : LightApple003.state = 1 : LightApple004.state = 1 : LightApple005.state = 1 : LightApple006.state = 1
	UpDateBGimage
End Sub

Sub CheckPieceCount
	If PieceCount(CurrentPlayer) <= 0 Then PieceCount(CurrentPlayer) = 0 : LightPiece001.state = 0 : LightPiece002.state = 0 : LightPiece003.state = 0 : LightPiece004.state = 0 : LightPiece005.state = 0 : LightPiece006.state = 0
	If PieceCount(CurrentPlayer) = 1 Then LightPiece001.state = 1 : LightPiece002.state = 0 : LightPiece003.state = 0 : LightPiece004.state = 0 : LightPiece005.state = 0 : LightPiece006.state = 0
	If PieceCount(CurrentPlayer) = 2 Then LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 0 : LightPiece004.state = 0 : LightPiece005.state = 0 : LightPiece006.state = 0 
	If PieceCount(CurrentPlayer) = 3 Then LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 1 : LightPiece004.state = 0 : LightPiece005.state = 0 : LightPiece006.state = 0 
	If PieceCount(CurrentPlayer) = 4 Then LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 1 : LightPiece004.state = 1 : LightPiece005.state = 0 : LightPiece006.state = 0
	If PieceCount(CurrentPlayer) = 5 Then LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 1 : LightPiece004.state = 1 : LightPiece005.state = 1 : LightPiece006.state = 0
	If PieceCount(CurrentPlayer) = 6 Then LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 1 : LightPiece004.state = 1 : LightPiece005.state = 1 : LightPiece006.state = 1 : StartMISultan : Playsound "Fx_Log" : DOF 126, DOFPulse  'DOF - Gear Motor
	If PieceCount(CurrentPlayer) >= 7 Then PieceCount(CurrentPlayer) = 6 : LightPiece001.state = 1 : LightPiece002.state = 1 : LightPiece003.state = 1 : LightPiece004.state = 1 : LightPiece005.state = 1 : LightPiece006.state = 1
	UpDateBGimage
End Sub

Sub CheckSnakeOK
	If Snake1ok(CurrentPlayer) = 0 Then Snake001.visible = 1
	If Snake1ok(CurrentPlayer) = 1 Then Snake001.visible = 0
	If Snake2ok(CurrentPlayer) = 0 Then Snake002.visible = 1
	If Snake2ok(CurrentPlayer) = 1 Then Snake002.visible = 0
	If Snake3ok(CurrentPlayer) = 0 Then Snake003.visible = 1
	If Snake3ok(CurrentPlayer) = 1 Then Snake003.visible = 0
	If Snake1ok(CurrentPlayer) = 1 And Snake2ok(CurrentPlayer) = 1 And Snake3ok(CurrentPlayer) = 1 And DoorON=False And (MBLock_Sav(CurrentPlayer) = 0 or MBLock_Sav(CurrentPlayer) = 2 or MBLock_Sav(CurrentPlayer) = 4) Then DoorONTimer.enabled=True : MBLock_Sav(CurrentPlayer) = MBLock_Sav(CurrentPlayer) + 1  : PlaySoundLockIsLit : CheckLightMB
	UpDateBGimage
End Sub

Sub CheckCoeurCount
	If CoeurCount(CurrentPlayer) <= 0 Then CoeurCount(CurrentPlayer) = 0 : LightCoeur001.state = 0 : LightCoeur002.state = 0 : LightCoeur003.state = 0 : LightCoeur004.state = 0
	If CoeurCount(CurrentPlayer) = 1 Then LightCoeur001.state = 1 : LightCoeur002.state = 0 : LightCoeur003.state = 0 : LightCoeur004.state = 0
	If CoeurCount(CurrentPlayer) = 2 Then LightCoeur001.state = 1 : LightCoeur002.state = 1 : LightCoeur003.state = 0 : LightCoeur004.state = 0
	If CoeurCount(CurrentPlayer) = 3 Then LightCoeur001.state = 1 : LightCoeur002.state = 1 : LightCoeur003.state = 1 : LightCoeur004.state = 0
	If CoeurCount(CurrentPlayer) = 4 Then LightCoeur001.state = 1 : LightCoeur002.state = 1 : LightCoeur003.state = 1 : LightCoeur004.state = 1 : Light008.state = 2
	If CoeurCount(CurrentPlayer) >= 5 Then LightCoeur001.state = 1 : LightCoeur002.state = 1 : LightCoeur003.state = 1 : LightCoeur004.state = 1 : Light008.state = 0
End Sub

Sub ResetCoeurCount
	CoeurCount(CurrentPlayer) = 0
	LightCoeur001.state = 0
	LightCoeur002.state = 0
	LightCoeur003.state = 0
	LightCoeur004.state = 0
	CheckCoeurCount
End Sub

Sub ResetSnakeApple
	Snake1ok(CurrentPlayer) = 0
	Snake2ok(CurrentPlayer) = 0
	Snake3ok(CurrentPlayer) = 0
	CheckSnakeOK
	AppleCount(CurrentPlayer) = 3
	CheckAppleCount
	UpDateBGimage
End Sub

Sub Trigger002_Hit()
	AddScore 550
	DOF 440, DOFPulse
	FlashBotelL.Visible = 0
	F1A008.state = 0
	RampCount = RampCount + 1
	If AppleCount(CurrentPlayer) <= 5 And Apple002.visible = True Then
	AppleCount(CurrentPlayer) = AppleCount(CurrentPlayer) + 1
	Playsound "Fx_Apple"
	Apple002.visible = 0
	CheckAppleCount
	End If
	If RaishaActive = 1 And LightRaisha004.state = 2 Then LightRaisha004.state = 1 : RaishaCount = RaishaCount - 1 : AdwardRaisha
End Sub

Sub Trigger003_Hit()
	AddScore 550
	DOF 442, DOFPulse
	CheckJump
	RampCount = RampCount + 1
	If AppleCount(CurrentPlayer) <= 5 And Apple003.visible = True Then
	AppleCount(CurrentPlayer) = AppleCount(CurrentPlayer) + 1
	Playsound "Fx_Apple"
	Apple003.visible = 0
	CheckAppleCount
	End If
	If RaishaActive = 1 And LightRaisha003.state = 2 Then LightRaisha003.state = 1 : RaishaCount = RaishaCount - 1 : AdwardRaisha
End Sub
'************ Moove Door
Dim DoorON
DoorON = False

Sub DoorONTimer_Timer()
If not DoorON Then
	CabanePorte.ObjRotZ = CabanePorte.ObjRotZ +2
	If CabanePorte.ObjRotZ = 100 Then
		If AnimTrailerTimer.Enabled = False Then Playsound "Fx_OpenDoor"
		DoorONTimer.enabled=False
		Wall016.collidable = False
		Wall017.collidable = True
		DoorON=True
	End IF
Else	
	If CabanePorte.ObjRotZ = 0 Then
		If AnimTrailerTimer.Enabled = False Then Playsound "Fx_CloseDoor"
		DoorONTimer.enabled=False
		Wall016.collidable = True
		Wall017.collidable = False
		DoorON=False
	Else
	CabanePorte.ObjRotZ = CabanePorte.ObjRotZ -2
	End IF
End If
End Sub
'***********Moove MegaTapis
Dim MegaTapisRight
MegaTapisRight = False

Sub MegaTapisTimer_TImer()
	
If not MegaTapisRight Then
	MegaTapis.ObjRotY = MegaTapis.ObjRotY +1
	MegaTapis001.ObjRotY = MegaTapis001.ObjRotY +1
	Light011.ObjRotY = Light011.ObjRotY +1
	If MegaTapis.ObjRotY = 7 Then
		MegaTapisTimer.enabled=False
		MegaTapisRight=False		
	End IF
Else	
	If MegaTapis.ObjRotY = -7 Then
		MegaTapisTimer.enabled=False
		MegaTapisRight=True
	Else
		MegaTapis.ObjRotY = MegaTapis.ObjRotY -1
		MegaTapis001.ObjRotY = MegaTapis001.ObjRotY -1
		Light011.ObjRotY = Light011.ObjRotY -1
	End IF
End If
End Sub

'Sub TapisLactive
'	If ModeMegaTapisActive = True And MegaTapisRight = True Then
''	PlaySoundCarMove
'	MegaTapisTimer.enabled=True	
'	MegaTapisRight = False	
'	End If
'End Sub

'Sub TapisRactive
'	If ModeMegaTapisActive = True And MegaTapisRight = False Then
''	PlaySoundCarMove
'	MegaTapisTimer.enabled=True
'	MegaTapisRight = True
'	End If
'End Sub
Sub Trigger008_Hit()
	AddScore2 355000
	Sabre001.Visible = 0
	StoryCount(CurrentPlayer) = 9
'	CheckStoryCount
	Playsound "Fx_Bonus1"
	if UseUltraDMD = 1 then UDMD "SWORD FOUND", "BONUS 355K", 3000
	pDMDmessage2L "Sword Found","Bonus 355 K",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	Trigger008.Enabled = False
End Sub 
Sub Trigger009_Hit()
	AddScore2 155000
	Diam001.Visible = 0
	discolight001.Visible = 0
	StoryCount(CurrentPlayer) = 3
'	CheckStoryCount
	Playsound "Fx_Bonus1"
	if UseUltraDMD = 1 then UDMD "RUBIS FOUND", "BONUS 155K", 3000
	pDMDmessage2L "Rubis Found","+ 155 K",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	Trigger009.Enabled = False
End Sub 
Sub Trigger010_Hit()
	AddScore2 255000
	Diam002.Visible = 0
	discolight002.Visible = 0
	StoryCount(CurrentPlayer) = 5
'	CheckStoryCount
	Playsound "Fx_Bonus1"
	if UseUltraDMD = 1 then UDMD "SAPHIR FOUND", "BONUS 255K", 3000
	pDMDmessage2L "Saphir Found","+ 255 K",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	Trigger010.Enabled = False
End Sub 

Sub StartMegaTapi 
	LightStoryOFFtps
	AladpieceRottimer.enabled=True
	If StoryCount(CurrentPlayer) = 2 Then Trigger008.Enabled = False : Trigger009.Enabled = True : Trigger010.Enabled = False : Diam001.Visible = 1 : discolight001.Visible = 1 : if UseUltraDMD = 1 then UDMD "FIND RUBIS", "", 2000'OBJET2 et Trigger visible
	If StoryCount(CurrentPlayer) = 4 Then Trigger008.Enabled = False : Trigger009.Enabled = False : Trigger010.Enabled = True : Diam002.Visible = 1 : discolight002.Visible = 1 : if UseUltraDMD = 1 then UDMD "FIND SAPHIR", "", 2000'OBJET3 et Trigger visible
	If StoryCount(CurrentPlayer) = 8 Then Trigger008.Enabled = True : Trigger009.Enabled = False : Trigger010.Enabled = False : Sabre001.Visible = 1 : if UseUltraDMD = 1 then UDMD "FIND SWORD", "", 2000'OBJET1 et Trigger visible
	GiSoutON
	GiOff
	DOF 127, DOFPulse 'blower
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 0 : GlowBatLightRight.State = 0
	RockA.Visible = 1
	RockB.Visible = 1
	FlasherPFA.Visible = False
	FlasherPFB.ImageA = "RampPFb2"
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	Kicker006.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker006.timerinterval = 3000
	kicker006.timerenabled = True
	ModeMegaTapisActive = True
	MegaTapis.Visible = 1
	MegaTapis001.Visible = 1
	MegaTapis.Collidable = 1
	MegaTapis002.Collidable = 1
	Target008.Visible = 0
	Target008.Collidable = 0
	Target009.Visible = 0
	Target009.Collidable = 0
	Target010.Visible = 0
	Target010.Collidable = 0
	RFLogo001.Visible = 0
	RightFlipper001.Enabled = 0
	Aladdin001.Visible = 0
	LampPr001.Visible = 0
	Wall015.Collidable = 0
	Wall037.Collidable = 1
	Wall051.Collidable = 0
	Wall052.Collidable = 0
	LiLifeBoss001.state = 0
	LiLifeBoss002.state = 0
	LiLifeBoss003.state = 0
	LiLifeBoss004.state = 0
	LiLifeBoss005.state = 0
	LiLifeAladd001.state = 0
	LiLifeAladd002.state = 0
	LiLifeAladd003.state = 0
End Sub
Sub EndMegaTapi
	CheckStoryCount
	If PieceCount(CurrentPlayer) >= 6 Then AladpieceRottimer.enabled=False
	Trigger008.Enabled = False
	Sabre001.Visible = 0
	Trigger009.Enabled = False
	Diam001.Visible = 0
	discolight001.Visible = 0
	Trigger010.Enabled = False
	Diam002.Visible = 0
	discolight002.Visible = 0
	GiSoutOFF
	GiOn
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 1 : GlowBatLightRight.State = 1
	RockA.Visible = 0
	RockB.Visible = 0
	FlashBotelL.Visible = 0
	FlashBotelR.Visible = 0
	F1A008.state = 0
	F1A005.state = 0
	FlasherPFA.Visible = True
	FlasherPFB.ImageA = "RampPFb"
	ModeMegaTapisActive = False
	MegaTapis.Visible = 0
	MegaTapis001.Visible = 0
	MegaTapis.Collidable = 0
	MegaTapis002.Collidable = 0
	Target008.Visible = 1
	Target008.Collidable = 1
	Target009.Visible = 1
	Target009.Collidable = 1
	Target010.Visible = 1
	Target010.Collidable = 1
	RFLogo001.Visible = 1
	RightFlipper001.Enabled = 1
	Aladdin001.Visible = 1
	LampPr001.Visible = 1
	Wall015.Collidable = 1
	Wall037.Collidable = 0
	Wall051.Collidable = 0
	Wall052.Collidable = 0
	TopMagnetR.MagnetOn = False
	TopMagnetL.MagnetOn = False
End Sub
'************ Moove Sultan/Najwa/Raisha/Tasmine
Dim SultanON, NajwaON, RaishaON, TasmineON
SultanON = False
NajwaON = False
RaishaON = False
TasmineON = False 

Sub SultanONTimer_Timer()
If not SultanON Then
	Sultan.TransY = Sultan.TransY +1
	CastelCenter.TransY = CastelCenter.TransY +1
	CastelCenter2.TransY = CastelCenter2.TransY +1
	TapiCoupolCenter.TransY = TapiCoupolCenter.TransY +1
	If Sultan.TransY = 140 Then
		SultanONTimer.enabled=False
		SultanON=True
	End IF
Else	
	If Sultan.TransY = 0 Then
		SultanONTimer.enabled=False
		SultanON=False
	Else
	Sultan.TransY = Sultan.TransY -2
	CastelCenter.TransY = CastelCenter.TransY -2
	CastelCenter2.TransY = CastelCenter2.TransY -2
	TapiCoupolCenter.TransY = TapiCoupolCenter.TransY -2
	End IF
End If
End Sub

Sub NajwaONTimer_Timer()
If not NajwaON Then
	Najwa.TransY = Najwa.TransY +1
	CastelCenter001.TransY = CastelCenter001.TransY +1
	TubeNajwa.TransY = TubeNajwa.TransY +1
	CastelMiniCoupol002.TransY = CastelMiniCoupol002.TransY +1
	If Najwa.TransY = 140 Then
		NajwaONTimer.enabled=False
		NajwaON=True
	End IF
Else	
	If Najwa.TransY = 0 Then
		NajwaONTimer.enabled=False
		NajwaON=False
	Else
	Najwa.TransY = Najwa.TransY -2
	CastelCenter001.TransY = CastelCenter001.TransY -2
	TubeNajwa.TransY = TubeNajwa.TransY -2
	CastelMiniCoupol002.TransY = CastelMiniCoupol002.TransY -2
	End IF
End If
End Sub

Sub RaishaONTimer_Timer()
If not RaishaON Then
	Raisha.TransY = Raisha.TransY +1
	CastelCenter002.TransY = CastelCenter002.TransY +1
	TubeRaisha.TransY = TubeRaisha.TransY +1
	CastelMiniCoupol001.TransY = CastelMiniCoupol001.TransY +1
	If Raisha.TransY = 140 Then
		RaishaONTimer.enabled=False
		RaishaON=True
	End IF
Else	
	If Raisha.TransY = 0 Then
		RaishaONTimer.enabled=False
		RaishaON=False
	Else
	Raisha.TransY = Raisha.TransY -2
	CastelCenter002.TransY = CastelCenter002.TransY -2
	TubeRaisha.TransY = TubeRaisha.TransY -2
	CastelMiniCoupol001.TransY = CastelMiniCoupol001.TransY -2
	End IF
End If
End Sub

Sub TasmineONTimer_Timer()
If not TasmineON Then
	Tasmine.TransY = Tasmine.TransY +1
	CastelCenter003.TransY = CastelCenter003.TransY +1
	TubeTasmine.TransY = TubeTasmine.TransY +1
	CastelMiniCoupol003.TransY = CastelMiniCoupol003.TransY +1
	If Tasmine.TransY = 140 Then
		TasmineONTimer.enabled=False
		TasmineON=True
	End IF
Else	
	If Tasmine.TransY = 0 Then
		TasmineONTimer.enabled=False
		TasmineON=False
	Else
	Tasmine.TransY = Tasmine.TransY -2
	CastelCenter003.TransY = CastelCenter003.TransY -2
	TubeTasmine.TransY = TubeTasmine.TransY -2
	CastelMiniCoupol003.TransY = CastelMiniCoupol003.TransY -2
	End IF
End If
End Sub

'*****begin HAL playing against himself************

sub trigger018_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger020.Enabled = True
        Trigger023.Enabled = True
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 1
Dim flipSounds1, flipPick1
flipSounds1 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick1 = Int(Rnd * (UBound(flipSounds1) + 1))
PlaySound flipSounds1(flipPick1)
timer065.enabled=1
end Sub

sub timer065_timer
LeftFlipper.RotateToStart
LeftFlipper001.RotateToStart
if ballsonplayfield > 1 Then
Timer065.interval=35
end if
if ballsonplayfield = 1 Then
Timer065.interval=100
end if
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
timer065.enabled=0
end Sub

sub trigger023_hit	
trigger018.enabled=1
Trigger019.enabled=1
Trigger023.enabled=0	
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 2
Dim flipSounds2, flipPick2
flipSounds2 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick2 = Int(Rnd * (UBound(flipSounds2) + 1))
PlaySound flipSounds2(flipPick2)

timer065.enabled=1
end Sub

sub trigger020_hit		
trigger018.enabled=1
Trigger019.enabled=1
trigger020.enabled=0
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 3
Dim flipSounds3, flipPick3
flipSounds3 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick3 = Int(Rnd * (UBound(flipSounds3) + 1))
PlaySound flipSounds3(flipPick3)

timer065.enabled=1
end Sub

sub trigger024_hit		
LeftFlipper.RotateToEnd
LeftFlipper001.RotateToEnd
' Flipper Sound Trigger 4
Dim flipSounds4, flipPick4
flipSounds4 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick4 = Int(Rnd * (UBound(flipSounds4) + 1))
PlaySound flipSounds4(flipPick4)

timer065.enabled=1
end Sub

sub trigger019_hit	
If Activeball.VelY < -5 Then Exit Sub
        Trigger022.Enabled = True
        Trigger021.Enabled = True
       RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 5
Dim flipSounds5, flipPick5
flipSounds5 = Array("flipper_l01", "flipper_l02", "flipper_l03", "flipper_l04", "flipper_l05", "flipper_l06", "flipper_l07", "flipper_l08", "flipper_l09", "flipper_l10", "flipper_l11")
flipPick5 = Int(Rnd * (UBound(flipSounds5) + 1))
PlaySound flipSounds5(flipPick5)

Timer066.enabled=1
end Sub

sub trigger022_hit	
trigger018.Enabled=1
Trigger019.Enabled=1	
trigger022.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 6
Dim flipSounds6, flipPick6
flipSounds6 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick6 = Int(Rnd * (UBound(flipSounds6) + 1))
PlaySound flipSounds6(flipPick6)

Timer066.enabled=1
end Sub

sub trigger021_hit	
trigger018.enabled=1
Trigger019.enabled=1	
trigger021.enabled=0
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
' Flipper Sound Trigger 7
Dim flipSounds7, flipPick7
flipSounds7 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick7 = Int(Rnd * (UBound(flipSounds7) + 1))
PlaySound flipSounds7(flipPick7)

Timer066.enabled=1
end Sub

sub trigger025_hit		
RightFlipper.RotateToEnd
RightFlipper001.RotateToEnd
Dim flipSounds8, flipPick8
flipSounds8 = Array("flipper_r01", "flipper_r02", "flipper_r03", "flipper_r04", "flipper_r05", "flipper_r06", "flipper_r07", "flipper_r08", "flipper_r09", "flipper_r10", "flipper_r11")
flipPick8 = Int(Rnd * (UBound(flipSounds8) + 1))
PlaySound flipSounds8(flipPick8)
Timer066.enabled=1
end Sub

sub Timer066_timer
RightFlipper.RotateToStart
RightFlipper001.RotateToStart
if ballsonplayfield > 1 Then
timer066.interval=35
end if
if ballsonplayfield = 1 Then
timer066.interval=100
end if
PlaySound SoundFX("fx_flipperdown",DOFFlippers), 0, 1, AudioPan(LeftFlipper), 0.05,0,0,1,AudioFade(LeftFlipper)
Timer066.enabled=0
end Sub

sub Kicker025_Hit 'hal kicker for autoplay
Plunger.AutoPlunger = True
kicker025.timerenabled=1
end sub

sub kicker025_timer 'hal kicker for autoplay
kicker025.kick 0, 40
'playsound "fx_kicker"
playsound "Plunger"'burger
playsoundat "warehousekick", Kicker025 'burger
lunger.fire
Plunger.AutoPlunger = False
kicker025.enabled=0
kicker025.timerenabled=0
end Sub

Sub Trigger029_Hit()
BumperLactive
End Sub
Sub Trigger030_Hit()
BumperRactive
End Sub

Sub halgame
	If bGameInPLay = False Then
		trigger018.enabled=1
		Trigger019.enabled=1
		Trigger020.enabled=1
		Trigger021.enabled=1
		Trigger022.enabled=1
		Trigger023.enabled=1
		Trigger024.enabled=1
		Trigger025.enabled=1
		Trigger029.enabled=1
		Trigger030.enabled=1
		Trigger026.enabled=1
		Kicker025.enabled=1
		Wall053.collidable = 1
		halplay=1
		ResetForNewGame()	
	End If
End sub

Sub HalgameOFF
	Trigger018.enabled=0
	Trigger019.enabled=0
	Trigger020.enabled=0
	Trigger021.enabled=0
	Trigger022.enabled=0
	Trigger023.enabled=0
	Trigger024.enabled=0
	Trigger025.enabled=0
	Trigger029.enabled=0
	Trigger030.enabled=0
	Trigger026.enabled=0
	kicker025.enabled=0
	kicker025.timerenabled=0
	Wall053.collidable = 0
	Plunger.AutoPlunger = False
	halplay=0
	EndOfGame()
	timer067.enabled=0
End Sub

'Sub timer067_timer
'halgame
'End Sub


Sub Trigger026_Hit  'when hal is playing and a ball doesnt clear the launch lane, this will shoot it back out
if halplay=1 then
    activeball.VelY = -60
    PlaySound "fx_kicker"
end if
End Sub

'*****end HAL playing against himself************

dim halplay
halplay = 0

'*****AnimBallScoop
Dim DropStep2, DropTimer2, vanishBall
DropStep2 = 0


Sub DropBallTimer_Timer()
    On Error Resume Next

    If DropStep2 < 2 Then
        If vanishBall Is Nothing Then
            DropBallTimer.Enabled = False
            Exit Sub
        End If

        vanishBall.Z = vanishBall.Z - 20
        DropStep2 = DropStep2 + 1
    Else
        If Not vanishBall Is Nothing Then vanishBall.Visible = False
        DropBallTimer.Enabled = False
        Set vanishBall = Nothing     ' <-- Clear reference here
    End If
End Sub


Sub StartDropAnimation(ball)
    Set vanishBall = ball
    DropStep2 = 0
    DropBallTimer.Enabled = True
End Sub

'*********TapiEVENT
Sub RampTrigger
    Dim ball
    Set ball = ActiveBall

    ' Vérifie si une balle est présente
    If Not ball Is Nothing Then
        ' Applique une impulsion en X et Y pour propulser la balle le long de la rampe
        ' Ajustez ces valeurs pour obtenir l'effet désiré
		If MegaTapis.ObjRotY = + 7 Then ball.VelX = +5
        If MegaTapis.ObjRotY = - 7 Then ball.VelX = -5
    End If
End Sub

Sub TapisLactive
	If ModeMegaTapisActive = True Then
		If MegaTapis.ObjRotY = 0 Then MegaTapis.ObjRotY = MegaTapis.ObjRotY + 7 : TopMagnetR.MagnetOn = False : TopMagnetL.MagnetOn = True': RampTrigger
		If MegaTapis001.ObjRotY = 0 Then MegaTapis001.ObjRotY = MegaTapis001.ObjRotY + 7
		If MegaTapis.ObjRotY = -7 Then MegaTapis.ObjRotY = MegaTapis.ObjRotY + 7 : TopMagnetR.MagnetOn = False : TopMagnetL.MagnetOn = False
		If MegaTapis001.ObjRotY = -7 Then MegaTapis001.ObjRotY = MegaTapis001.ObjRotY + 7
	End If
End Sub

Sub TapisRactive
	If ModeMegaTapisActive = True Then
		If MegaTapis.ObjRotY = 0 Then MegaTapis.ObjRotY = MegaTapis.ObjRotY - 7 : TopMagnetL.MagnetOn = False : TopMagnetR.MagnetOn = True': RampTrigger
		If MegaTapis001.ObjRotY = 0 Then MegaTapis001.ObjRotY = MegaTapis001.ObjRotY - 7
		If MegaTapis.ObjRotY = +7 Then MegaTapis.ObjRotY = MegaTapis.ObjRotY - 7 : TopMagnetR.MagnetOn = False : TopMagnetL.MagnetOn = False
		If MegaTapis001.ObjRotY = +7 Then MegaTapis001.ObjRotY = MegaTapis001.ObjRotY - 7
	End If
End Sub

Sub MegaTapis_Hit()
'	cvpmMech Me.HitEvent
    Dim ball
    Set ball = ActiveBall

    If Not ball Is Nothing Then
        ' Applique une faible impulsion pour freiner la balle
        ' Ajustez cette valeur pour obtenir une vitesse de roulement plus lente
  '      ball.VelY = -50 ' Ou une autre valeur négative plus petite que -200
		ball.VelY = ball.VelY * 0.5
    End If
End Sub

Sub ChangePalmOFF
	LFLogo.visible=1:RFLogo.visible=1
	FlipperLSh.visible = 1 : FlipperRSh.visible = 1
	glowbatleft.visible = 0 : glowbatright.visible = 0 : GlowBatLightLeft.state = 0 : GlowBatLightRight.state = 0
End Sub
Sub ChangePalmGreen
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat green" : glowbatright.image = "glowbat green" 
	GlowBatLightLeft.color = RGB(0,255,0) : GlowBatLightLeft.colorfull = RGB(0,255,0)
	GlowBatLightRight.color = RGB(0,255,0): GlowBatLightRight.colorfull = RGB(0,255,0)
	End If
End Sub
Sub ChangePalmRed
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Red" : glowbatright.image = "glowbat Red" 
	GlowBatLightLeft.color = RGB(255,0,0) : GlowBatLightLeft.colorfull = RGB(255,0,0)
	GlowBatLightRight.color = RGB(255,0,0): GlowBatLightRight.colorfull = RGB(255,0,0)
	End If
End Sub
Sub ChangePalmBlue
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Blue" : glowbatright.image = "glowbat Blue" 
	GlowBatLightLeft.color = RGB(0,128,255) : GlowBatLightLeft.colorfull = RGB(0,128,255)
	GlowBatLightRight.color = RGB(0,128,255): GlowBatLightRight.colorfull = RGB(0,128,255)
	End If
End Sub
Sub ChangePalmPink
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Pink" : glowbatright.image = "glowbat Pink" 
	GlowBatLightLeft.color = RGB(255,0,128) : GlowBatLightLeft.colorfull = RGB(255,0,128)
	GlowBatLightRight.color = RGB(255,0,128): GlowBatLightRight.colorfull = RGB(255,0,128)
	End If
End Sub
Sub ChangePalmYel
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Yel" : glowbatright.image = "glowbat Yel" 
	GlowBatLightLeft.color = RGB(255,255,0) : GlowBatLightLeft.colorfull = RGB(255,255,0)
	GlowBatLightRight.color = RGB(255,255,0): GlowBatLightRight.colorfull = RGB(255,255,0)
	End If
End Sub
Sub ChangePalmBICOL
	If ModeFlipGlowActive = 0 Then
	LFLogo.visible=0:RFLogo.visible=0
	FlipperLSh.visible = 0 : FlipperRSh.visible = 0
	glowbatleft.visible = 1 : glowbatright.visible = 1 : GlowBatLightLeft.state = 1 : GlowBatLightRight.state = 1 
	glowbatleft.image = "glowbat Blue" : glowbatright.image = "glowbat Pink" 
	GlowBatLightLeft.color = RGB(0,128,255) : GlowBatLightLeft.colorfull = RGB(0,128,255)
	GlowBatLightRight.color = RGB(255,0,128): GlowBatLightRight.colorfull = RGB(255,0,128)
	End If
End Sub
'--------
'************ Moove Boss Under2
' --- Variables de contrôle ---
Dim AnimStep : AnimStep = 0
Dim AnimSubStep : AnimSubStep = 0 ' Pour gérer les répétitions (bras)
Dim AnimWait : AnimWait = 0

Sub StartBossAnim()
    AnimStep = 1
    AnimSubStep = 0
    AnimationBossTimer.Enabled = True
End Sub

Sub AnimationBossTimer_Timer()
    Select Case AnimStep
        
        ' 1 & 2) Animation du Bras (2 cycles de haut/bas)
        Case 1, 2 
            If AnimSubStep < 50 Then
                Boss002.RotX = Boss002.RotX + 10 : AnimSubStep = AnimSubStep + 10    
            ElseIf AnimSubStep < 100 Then
                Boss002.RotX = Boss002.RotX - 2 : AnimSubStep = AnimSubStep + 2
            Else
                AnimSubStep = 0
                AnimStep = AnimStep + 1 
				If MooveAladdinBossnr = 3 Then LifeAladdCount = LifeAladdCount + 1 : CkeckLightBoss2 : Playsound"fx_Boss3Aladdin" : MooveAladdinBossnr = 2
            End If

        ' 3) Rotation +180 ET Décalage TransX
        Case 3
            If Boss001.RotY < 180 Then
                Boss001.RotY = Boss001.RotY + 10 : Boss002.RotY = Boss002.RotY + 10
                Boss001.TransX = Boss001.TransX + 2 : Boss002.TransX = Boss002.TransX + 2
            Else
                ' On fixe les valeurs exactes pour éviter les décalages
                Boss001.RotY = 180 : Boss001.TransX = 36 
                Boss002.RotY = 180 : Boss002.TransX = 36 
                
                ' On prépare la temporisation
                AnimWait = 25 ' 25 = Environ 1 seconde de pause
                AnimStep = 40 ' On passe à l'étape d'attente
            End If

        ' 40) ÉTAPE DE TEMPORISATION (Attente avant descente)
        Case 40
            If AnimWait > 0 Then
                AnimWait = AnimWait - 1
            Else
                AnimStep = 4 ' Le temps est écoulé, on descend
            End If

        ' 4) Montée (qui est ici une descente vers le bas du plateau)
        Case 4
            If Boss001.TransZ > -50 Then  
                Boss001.TransZ = Boss001.TransZ - 5 : Boss002.TransZ = Boss002.TransZ - 5
            Else
                Boss001.TransZ = -50 : Boss002.TransZ = -50
                AnimStep = 5
            End If

        ' 5) LE SNAP (Retournement instantané)
        ' On réinitialise la rotation et on téléporte en haut pour la redescente
        Case 5
			Playsound "fx_Boss3Return"
            Boss001.RotY = 0 : Boss001.TransX = 0
            Boss002.RotY = 0 : Boss002.TransX = 0
            Boss001.TransZ = 50 : Boss002.TransZ = 50 
            AnimStep = 6 

        ' 6) Redescente finale au point d'origine
        Case 6
            If Boss001.TransZ > 0 Then
                Boss001.TransZ = Boss001.TransZ - 5 : Boss002.TransZ = Boss002.TransZ - 5
            Else
                ' Reset complet avant de recommencer la boucle
                Boss001.TransZ = 0 : Boss002.TransZ = 0
                AnimStep = 1 
                AnimSubStep = 0
				If MooveAladdinBossnr = 3 Then LifeAladdCount = LifeAladdCount + 1 : CkeckLightBoss2 : Playsound"fx_Boss3Aladdin" : MooveAladdinBossnr = 2
            End If

    End Select
End Sub

Dim MooveAladdinBossnr : MooveAladdinBossnr = 2 ' Position Neutre par défaut
Dim AladdinAnimStep : AladdinAnimStep = 0
Dim AladdinWait : AladdinWait = 0

Sub AladdinMooveBoss(keycode)

    If keycode = RightMagnaSave Then
        If MooveAladdinBossnr = 3 Then 
            MooveAladdinBossnr = 4 
            AladdinAnimStep = 0 ' Reset l'anim de rotation
			If AnimStep = 40 Then LifeBoss2count = LifeBoss2count + 1 : PlaySoundBoss3 : CkeckLightBoss2
        End If
    End If

    ' Flips : Navigation bloquée 1 <-> 2 <-> 3
    If keycode = LeftFlipperKey Then 
		If PikBossActive = 0 Then ChangeAladdinPos -1
'		If PikBossActive = 1 And MooveAladdinBossnr = 2 Then LifeAladdCount = LifeAladdCount + 1 : CkeckLightBoss2 : Playsound"" : MooveAladdinBossnr = 1
	End If 
    If keycode = RightFlipperKey Then 
		If PikBossActive = 0 Then ChangeAladdinPos 1
'		If PikBossActive = 1 And MooveAladdinBossnr = 2 Then LifeAladdCount = LifeAladdCount + 1 : CkeckLightBoss2 : Playsound"" : MooveAladdinBossnr = 1
	End If
End Sub

Sub ChangeAladdinPos(dir)
    ' dir = -1 pour Gauche, +1 pour Droite
    
    If dir = -1 Then ' Tentative d'aller à GAUCHE
        If MooveAladdinBossnr = 2 Then 
            MooveAladdinBossnr = 1 ' Du neutre vers la gauche
        ElseIf MooveAladdinBossnr = 3 Then 
            MooveAladdinBossnr = 2 ' Du haut vers le neutre
        ' Si on est déjà en 1, il ne se passe rien
        End If
    End If

    If dir = 1 Then ' Tentative d'aller à DROITE
        If MooveAladdinBossnr = 2 Then 
            MooveAladdinBossnr = 3 ' Du neutre vers le haut
        ElseIf MooveAladdinBossnr = 1 Then 
            MooveAladdinBossnr = 2 ' De la gauche vers le neutre
        ' Si on est déjà en 3, il ne se passe rien (ton souhait)
        End If
    End If
End Sub

Sub AladdinBossTimer_Timer()
    Select Case MooveAladdinBossnr
        
        Case 1 ' GAUCHE
            If Mode3Aladd.TransX > -50 Then Mode3Aladd.TransX = Mode3Aladd.TransX - 5
            If Mode3Aladd.TransZ > 0 Then Mode3Aladd.TransZ = Mode3Aladd.TransZ - 5

        Case 2 ' NEUTRE
            ' On ramène X et Z vers 0
            If Mode3Aladd.TransX < 0 Then Mode3Aladd.TransX = Mode3Aladd.TransX + 5
            If Mode3Aladd.TransX > 0 Then Mode3Aladd.TransX = Mode3Aladd.TransX - 5
            If Mode3Aladd.TransZ < 0 Then Mode3Aladd.TransZ = Mode3Aladd.TransZ + 5
            If Mode3Aladd.TransZ > 0 Then Mode3Aladd.TransZ = Mode3Aladd.TransZ - 5

        Case 3 ' HAUT (TransZ +50)
            If Mode3Aladd.TransZ < 50 Then Mode3Aladd.TransZ = Mode3Aladd.TransZ + 5
            If Mode3Aladd.TransX < 0 Then Mode3Aladd.TransX = Mode3Aladd.TransX + 5

        Case 4 ' ACTION MAGNASAVE (Rotation Y)
            Select Case AladdinAnimStep
                Case 0 ' Aller +20
                    If Mode3Aladd.RotY < 20 Then Mode3Aladd.RotY = Mode3Aladd.RotY + 5 Else AladdinAnimStep = 1
                Case 1 ' Retour -20
                    If Mode3Aladd.RotY > -20 Then Mode3Aladd.RotY = Mode3Aladd.RotY - 5 Else AladdinAnimStep = 2
                Case 2 ' Aller +20 (2ème fois)
                    If Mode3Aladd.RotY < 20 Then Mode3Aladd.RotY = Mode3Aladd.RotY + 5 Else AladdinAnimStep = 3
                Case 3 ' Retour à 0
                    If Mode3Aladd.RotY > 0 Then 
					Mode3Aladd.RotY = Mode3Aladd.RotY - 5 
				Else 
                   Mode3Aladd.RotY = 0
                   MooveAladdinBossnr = 3 ' Revient en position 3 automatiquement
                End If
            End Select
    End Select
End Sub

Dim PikBossActive
PikBossActive = 0

Sub PikBossTimer001_Timer()
	Mode3Pique002.visible = 1
	PikBossActive = 1
	If PikBossActive = 1 And MooveAladdinBossnr = 2 Then LifeAladdCount = LifeAladdCount + 1 : CkeckLightBoss2 : Playsound"fx_Boss3Aladdin" : MooveAladdinBossnr = 1
	PikBossTimer002.Enabled = True
End Sub
Sub PikBossTimer002_Timer()
	Mode3Pique002.visible = 0
	PikBossActive = 0
	PikBossTimer002.Enabled = False
End Sub

Sub StartBoss
	PuPEvent 910
	AddScore 15000
	StartBossAnim
	bAladdinBoss = True
	AladdinBossTimer.Enabled = True
	PikBossActive = 0
	PikBossTimer001.Enabled = True
	LifeBoss2count = 0
	LifeAladdCount = 0
	Mode3001.visible = 1
	Mode3002.visible = 1
	Mode3003.visible = 1
	Mode3Aladd.visible = 1
	Mode3Pique001.visible = 1
	Mode3Pique002.visible = 0
	Boss001.visible = 1
	Boss002.visible = 1
	LiLifeBoss001.state = 1
	LiLifeBoss002.state = 1
	LiLifeBoss003.state = 1
	LiLifeBoss004.state = 1
	LiLifeBoss005.state = 1
	LiLifeAladd001.state = 1
	LiLifeAladd002.state = 1
	LiLifeAladd003.state = 1
	GiSoutON
	GiOff
'	DOF 127, DOFPulse 'blower
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 0 : GlowBatLightRight.State = 0
	RockA.Visible = 0
	RockB.Visible = 0
	FlasherPFA.Visible = False
	FlasherPFB.ImageA = "RampPFb7"
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
'	Kicker006.CreateSizedBallWithMass BallSize / 2, BallMass
'	Kicker006.timerinterval = 3000
'	kicker006.timerenabled = True
	Target008.Visible = 0
	Target008.Collidable = 0
	Target009.Visible = 0
	Target009.Collidable = 0
	Target010.Visible = 0
	Target010.Collidable = 0
	RFLogo001.Visible = 0
	RightFlipper001.Enabled = 0
	Aladdin001.Visible = 0
	LampPr001.Visible = 0
	Wall015.Collidable = 0
	Wall037.Collidable = 1
	Wall051.Collidable = 0
	Wall052.Collidable = 0
End Sub
Sub CkeckLightBoss2
	If LifeBoss2count = 0 Then LiLifeBoss001.state = 1 : LiLifeBoss002.state = 1 : LiLifeBoss003.state = 1 : LiLifeBoss004.state = 1 : LiLifeBoss005.state = 1
	If LifeBoss2count = 1 Then LiLifeBoss001.state = 1 : LiLifeBoss002.state = 1 : LiLifeBoss003.state = 1 : LiLifeBoss004.state = 1 : LiLifeBoss005.state = 0
	If LifeBoss2count = 2 Then LiLifeBoss001.state = 1 : LiLifeBoss002.state = 1 : LiLifeBoss003.state = 1 : LiLifeBoss004.state = 0 : LiLifeBoss005.state = 0
	If LifeBoss2count = 3 Then LiLifeBoss001.state = 1 : LiLifeBoss002.state = 1 : LiLifeBoss003.state = 0 : LiLifeBoss004.state = 0 : LiLifeBoss005.state = 0
	If LifeBoss2count = 4 Then LiLifeBoss001.state = 1 : LiLifeBoss002.state = 0 : LiLifeBoss003.state = 0 : LiLifeBoss004.state = 0 : LiLifeBoss005.state = 0
	If LifeBoss2count = 5 Then LiLifeBoss001.state = 0 : LiLifeBoss002.state = 0 : LiLifeBoss003.state = 0 : LiLifeBoss004.state = 0 : LiLifeBoss005.state = 0 : StopBossWin
	If LifeAladdCount = 0 Then LiLifeAladd001.state = 1 : LiLifeAladd002.state = 1 : LiLifeAladd003.state = 1
	If LifeAladdCount = 1 Then LiLifeAladd001.state = 1 : LiLifeAladd002.state = 1 : LiLifeAladd003.state = 0
	If LifeAladdCount = 2 Then LiLifeAladd001.state = 1 : LiLifeAladd002.state = 0 : LiLifeAladd003.state = 0
	If LifeAladdCount = 3 Then LiLifeAladd001.state = 0 : LiLifeAladd002.state = 0 : LiLifeAladd003.state = 0 : StopBossLose
End Sub
Sub StopBossLose
	PuPEvent 911
	Addscore2 8000
	AladdinBossTimer.Enabled = False
	AnimationBossTimer.Enabled = False
	PikBossTimer001.Enabled = False
	PikBossActive = 0
	bAladdinBoss = False
	Kicker006.CreateSizedBallWithMass BallSize / 2, BallMass
	Kicker006.timerinterval = 3000
	kicker006.timerenabled = True
	Boss001.RotY = 0
	Boss001.TransX = 0
	Boss001.TransZ = 0
    Boss002.RotY = 0
	Boss002.TransX = 0
    Boss002.TransZ = 0
	Mode3001.visible = 0
	Mode3002.visible = 0
	Mode3003.visible = 0
	Mode3Aladd.visible = 0
	Mode3Pique001.visible = 0
	Mode3Pique002.visible = 0
	Boss001.visible = 0
	Boss002.visible = 0
	LifeBoss2count = 0
	LifeAladdCount = 0
	LiLifeBoss001.state = 0
	LiLifeBoss002.state = 0
	LiLifeBoss003.state = 0
	LiLifeBoss004.state = 0
	LiLifeBoss005.state = 0
	LiLifeAladd001.state = 0
	LiLifeAladd002.state = 0
	LiLifeAladd003.state = 0
	GiSoutON
	GiOff
	If ModeFlipGlowActive = 0 Then GlowBatLightLeft.State = 1 : GlowBatLightRight.State = 1
	RockA.Visible = 0
	RockB.Visible = 0
	FlashBotelL.Visible = 0
	FlashBotelR.Visible = 0
	F1A008.state = 0
	F1A005.state = 0
	FlasherPFA.Visible = True
	FlasherPFB.ImageA = "RampPFb"
'	RightFlipper001.Enabled = 1
End Sub
Sub StopBossWin
	StoryCount(CurrentPlayer) = StoryCount(CurrentPlayer) +1
	Addscore 450000
	pDMDmessage2L "Boss Dead","+ 450 k",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	if UseUltraDMD = 1 then UDMD "GOSS DEAD", "450k", 3000
	PlaySoundWinMI
	StopBossLose
End Sub

Sub StartGenieFree
	StoryCount(CurrentPlayer) = 12
	bModeGenieFree = 1
	AddScore2 650000
	ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
	PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
	ResetSultanGirls
	BonusMultiplier = 5
	PlayfieldMultiplier = 5
	ChangeGi blue
	ChangeBall(1) 'bleu
	ChangePalmBlue
	DOF 324, DOFOn
	pDMDmessage2L "Genie Happyu","+ 650 k",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	if UseUltraDMD = 1 then UDMD "GENIE HAPPY", "650k", 3000
	PlaySong "Mu_MB"
	PauseDMDHarem = 1
'	PuPEvent 806
	EnableBallSaver 45
	WizardGenieTimer.Enabled=True
	vpmtimer.addtimer 3000, "AddMB1 '"
	vpmtimer.addtimer 6000, "AddMB1 '"
End Sub

Sub WizardGenieTimer_Timer()
	GiOff
	FlashBotelL.Visible = 1
	FlashBotelR.Visible = 1
	FlashForMs F1A008, 500, 100, 1
	FlashForMs F1A005, 500, 100, 1
	bModeGenieFree = 2
	pDMDmessage2L "Wizard Mode","Timer End",1
    pDMDLabelFadePulse "Texte2b", 2500, clGold
	if UseUltraDMD = 1 then UDMD "WIZARD", "TIMER END", 3000
	WizardGenieTimer.Enabled=False
End Sub

Sub StopGenieFree
	If bModeGenieFree >= 1 Then
		WizardGenieTimer.Enabled=False
		bModeGenieFree = 0
		ChangeGi white
		ChangeBall(0) 
		ChangePalmBICOL
		FlashBotelL.Visible = 0
		FlashBotelR.Visible = 0
		F1A008.state = 0
		F1A005.state = 0
		DOF 324, DOFOff
	End If
End Sub
'-----------------
'--- Wheel Bonus
'--------------------
Dim RecompenseEnAttente ' Stocke le type de gain choisi
Dim ValeurEnAttente      ' Stocke la valeur (points, etc.)

Sub LancerRoueAladdin()
    Dim alea
    Dim DureeVideo
    DureeVideo = 10000 ' Ajuste ici la durée exacte de tes vidéos (en ms) 
	PauseDMDHarem = 1
	PauseDMDHaremTimer.Enabled = True
	UpdateDMDHarem
	StartFlasherApron

    Randomize
    alea = Rnd() * 100

    ' --- SÉLECTION ALÉATOIRE ---
    If alea < 30 Then 
        RecompenseEnAttente = "Points" : ValeurEnAttente = 1000 : Pupevent 904 'pain points 1k 					30%
    ElseIf alea < 50 Then        
		RecompenseEnAttente = "Sultan" : ValeurEnAttente = 1 : Pupevent 902 ' Piece sultant + 1					20%
    ElseIf alea < 65 Then 
        RecompenseEnAttente = "KickBack" : ValeurEnAttente = 1 : Pupevent 907 'Scarabé KickBack Actived			15%
    ElseIf alea < 80 Then 
		RecompenseEnAttente = "BallSaveOk" : ValeurEnAttente = 0 : Pupevent 905 ' Tapis - Balle save Actived	15%
    ElseIf alea < 90 Then 
		RecompenseEnAttente = "SablierMode" : ValeurEnAttente = 1 : Pupevent 903 'Sablier 30s bumper hits x5	10%
    ElseIf alea < 95 Then 
		RecompenseEnAttente = "LampActive" : ValeurEnAttente = 0 : Pupevent 906 ' Lampe voeux génie Activé		5% 
    ElseIf alea < 97 Then 
        RecompenseEnAttente = "Points" : ValeurEnAttente = 500000 : Pupevent 901 'Emmeraude points 500k			3%	
    Else 
        RecompenseEnAttente = "ExtraBall" : ValeurEnAttente = 0 : Pupevent 908 'Genie Extraball					2%
    End If

    ' --- LANCEMENT DU TIMER ---
    ' On attend la fin de la vidéo avant d'exécuter la récompense
    vpmTimer.AddTimer DureeVideo, "AttribuerGainRoue '"
'	vpmTimer.AddTimer DureeVideo, "AttribuerGainRoue"
End Sub

Sub AttribuerGainRoue()
    Select Case RecompenseEnAttente
        Case "Points"
            AddScore2 ValeurEnAttente
 '           PlaySound "fx_points"
            if UseUltraDMD = 1 then UDMD "BONUS", "POINT", 3000

        Case "Sultan"
			PieceCount(CurrentPlayer) = PieceCount(CurrentPlayer) + ValeurEnAttente
			pDMDLabelPulseText2 "PieceAdd","+1",2000,clGreen
			CheckPieceCount
            if UseUltraDMD = 1 then UDMD "COLLECT", "1 COIN", 3000
			pDMDmessage2L "Sultan","+ 1 coin",1
			pDMDLabelFadePulse "Texte2b", 2500, clGold

        Case "KickBack"
            LeftOutlane.State=1
			LeftInlane.State=1
			RightInlane.State=1
			RightOutlane.State=1
			DOF 338, DOFPulse
			KickBackActive(CurrentPlayer) = True
            if UseUltraDMD = 1 then UDMD "KICKBACK", "ACTIVE", 3000
			pDMDmessage2L "KickBack","Active",1
			pDMDLabelFadePulse "Texte2b", 2500, clGold

        Case "BallSaveOk"
			EnableBallSaver 15
            if UseUltraDMD = 1 then UDMD "BALLSAVE", "ACTIVE", 3000
			pDMDmessage2L "BallSave","Active",1
			pDMDLabelFadePulse "Texte2b", 2500, clGold

        Case "SablierMode"
            ModeSablier = True
			ChangeBall(3) 'rouge
			ChangePalmRed
			ModeSablierTimer.Enabled = True
			if UseUltraDMD = 1 then UDMD "BUMPER HIT", "X 5 30S", 3000
			pDMDmessage3L "Mode","Bumper Hit X5","Timer 30s",1
		    pDMDLabelFadePulse "Texte3b", 2500, clGold
			pDMDLabelFadePulse "Texte3c", 2500, clGold

		Case "LampActive"
            SpinnerCount(CurrentPlayer) = 0
			ModeSpinnersActive(CurrentPlayer) = False
			MysteryState(CurrentPlayer) = 2
			Light010.state = 0
			MysteryActivedTimer.enabled = True
			if UseUltraDMD = 1 then UDMD "GENIE", "IS LIT", 3000
			pDMDmessage2L "Wish Genie","is Lit",1
			pDMDLabelFadePulse "Texte2a", 2500, clGold
			pDMDLabelFadePulse "Texte2b", 2500, clGold

		Case "ExtraBall"
			ExtraBallsAwards(CurrentPlayer) = ExtraBallsAwards(CurrentPlayer) + 1
			PlaySound SoundFXDOF("fx_Knocker", 122, DOFPulse, DOFKnocker)
			pDMDmessage1L "Extraball",1
			pDMDLabelFadePulse "Texte1a", 2500, clGreen
			if UseUltraDMD = 1 then UDMD "EXTRABALL", "", 3000
'			AwardExtraBall
 
    End Select
    ' On vide les variables pour la prochaine fois
	StopFlasherApron
    RecompenseEnAttente = ""
    ValeurEnAttente = 0
End Sub

Sub ModeSablierTimer_Timer()
	pDMDmessage3L "Mode","Bumper X","Timer End",1
    pDMDLabelFadePulse "Texte3b", 2500, clGold
	ChangeBall(0) 'off
	if UseUltraDMD = 1 then UDMD "BUMPER X END", "", 3000
	ModeSablierTimer.Enabled = False
End Sub

Sub StartFlasherApron
	If usePuPDMD=True Then
		l001.state = 0
		l002.state = 0
		l003.state = 0
		l004.state = 0
		pupflasher.visible=True
		pupflasher.VideoCapWidth= 400		
		pupflasher.VideoCapHeight=186
		pupflasher.TimerEnabled = True
	End If
End Sub

Sub StopFlasherApron
	If usePuPDMD=True Then
		l001.state = 1
		l002.state = 1
		l003.state = 1
		l004.state = 1
		pupflasher.visible=False
		pupflasher.TimerEnabled = False
	End If
End Sub

Sub pupflasher_Timer()
	pupflasher.VideoCapUpdate="PUPSCREEN12"
End Sub


'******************************************************
'	ZBRL: BALL ROLLING AND DROP SOUNDS
'******************************************************
'
' Be sure to call RollingUpdate in a timer with a 10ms interval see the GameTimer_Timer() sub

ReDim rolling(tnob)
InitRolling

Dim DropCount
ReDim DropCount(tnob)

Sub InitRolling
	Dim i
	For i = 0 to tnob
		rolling(i) = False
	Next
End Sub

Sub RollingUpdate()
	Dim b', BOT
	gBOT = GetBalls

	' stop the sound of deleted balls
	For b = UBound(gBOT) + 1 to tnob - 1
		rolling(b) = False
		StopSound("BallRoll_" & b)
	Next

	' exit the sub if no balls on the table
	If UBound(gBOT) = -1 Then Exit Sub

	' play the rolling sound for each ball
	For b = 0 to UBound(gBOT)

		'If BallVel(gBOT(b)) > 1 AND gBOT(b).z < 30 Then  'burger
		If BallVel(gBOT(b)) > 1 AND (gBOT(b).Z < 30 OR gBOT(b).Z >= 214) Then
			rolling(b) = True
			PlaySound ("BallRoll_" & b), -1, VolPlayfieldRoll(gBOT(b)) * BallRollVolume * VolumeDial, AudioPan(gBOT(b)), 0, PitchPlayfieldRoll(gBOT(b)), 1, 0, AudioFade(gBOT(b))
		Else
			If rolling(b) = True Then
				StopSound("BallRoll_" & b)
				rolling(b) = False
			End If
		End If

		' Ball Drop Sounds
		If gBOT(b).VelZ < -1 and gBOT(b).z < 55 and gBOT(b).z > 27 Then 'height adjust for ball drop sounds
			If DropCount(b) >= 5 Then
				DropCount(b) = 0
				If gBOT(b).velz > -7 Then
					RandomSoundBallBouncePlayfieldSoft gBOT(b)
				Else
					RandomSoundBallBouncePlayfieldHard gBOT(b)
				End If				
			End If
		End If
		If DropCount(b) < 5 Then
			DropCount(b) = DropCount(b) + 1
		End If

		' --- moved block START (only change: now inside loop so b is valid)
		If broomdone = 0 Then
			' 1. On vérifie si la bille est dans le couloir (X > 920)
			' 2. On vérifie si elle est entre le bas (2190) et le haut (1005) et sous la haut du haut
			If gBOT(b).X > 920 And gBOT(b).Y > 1005 And gBOT(b).Y < 2190 And gBOT(b).Z < 120 Then
        
				' La primitive suit la bille en Y
				Tapisbroom.Y = gBOT(b).Y
        
				' FORCE le Z à rester à 4 pour éviter qu'il ne s'envole à l'initialisation
				If gBOT(b).Y < 1560 Then Tapisbroom.Z = gBOT(b).Z Else Tapisbroom.Z = 4 

				' DETECTION DU SOMMET : 
				' Si la bille dépasse le point 1005 (elle devient plus petite que 1005)
				If gBOT(b).Y <= 1110 Then 
					broomdone = 1
					Tapisbreturn.Enabled = True ' Active le timer de retour
					debug.print "Retour activé !"
				End If
			End If
		End If
		' --- moved block END

	Next
End Sub

'******************************************************
'****  END BALL ROLLING AND DROP SOUNDS
'******************************************************

'******************************************************
' 	ZRRL: RAMP ROLLING SFX
'******************************************************

'Ball tracking ramp SFX 1.0
'   Reqirements:
'          * Import A Sound File for each ball on the table for plastic ramps.  Call It RampLoop<Ball_Number> ex: RampLoop1, RampLoop2, ...
'          * Import a Sound File for each ball on the table for wire ramps. Call it WireLoop<Ball_Number> ex: WireLoop1, WireLoop2, ...
'          * Create a Timer called RampRoll, that is enabled, with a interval of 100
'          * Set RampBAlls and RampType variable to Total Number of Balls
'	Usage:
'          * Setup hit events and call WireRampOn True or WireRampOn False (True = Plastic ramp, False = Wire Ramp)
'          * To stop tracking ball
'                 * call WireRampOff
'                 * Otherwise, the ball will auto remove if it's below 30 vp units
'

dim RampMinLoops : RampMinLoops = 4

' RampBalls
'      Setup:        Set the array length of x in RampBalls(x,2) Total Number of Balls on table + 1:  if tnob = 5, then RammBalls(6,2)
'      Description:  
dim RampBalls(5,2)
'x,0 = ball x,1 = ID,	2 = Protection against ending early (minimum amount of updates)
'0,0 is boolean on/off, 0,1 unused for now
RampBalls(0,0) = False

' RampType
'     Setup: Set this array to the number Total number of balls that can be tracked at one time + 1.  5 ball multiball then set value to 6
'     Description: Array type indexed on BallId and a values used to deterimine what type of ramp the ball is on: False = Wire Ramp, True = Plastic Ramp
dim RampType(5)	

Sub WireRampOn(input)  : Waddball ActiveBall, input : RampRollUpdate: End Sub
Sub WireRampOff() : WRemoveBall ActiveBall.ID	: End Sub


' WaddBall (Active Ball, Boolean)
'     Description: This subroutine is called from WireRampOn to Add Balls to the RampBalls Array
Sub Waddball(input, RampInput)	'Add ball
	' This will loop through the RampBalls array checking each element of the array x, position 1
	' To see if the the ball was already added to the array.
	' If the ball is found then exit the subroutine
	dim x : for x = 1 to uBound(RampBalls)	'Check, don't add balls twice
		if RampBalls(x, 1) = input.id then 
			if Not IsEmpty(RampBalls(x,1) ) then Exit Sub	'Frustating issue with BallId 0. Empty variable = 0
		End If
	Next

	' This will itterate through the RampBalls Array.
	' The first time it comes to a element in the array where the Ball Id (Slot 1) is empty.  It will add the current ball to the array
	' The RampBalls assigns the ActiveBall to element x,0 and ball id of ActiveBall to 0,1
	' The RampType(BallId) is set to RampInput
	' RampBalls in 0,0 is set to True, this will enable the timer and the timer is also turned on
	For x = 1 to uBound(RampBalls)
		if IsEmpty(RampBalls(x, 1)) then 
			Set RampBalls(x, 0) = input
			RampBalls(x, 1)	= input.ID
			RampType(x) = RampInput
			RampBalls(x, 2)	= 0
			'exit For
			RampBalls(0,0) = True
			RampRoll.Enabled = 1	 'Turn on timer
			'RampRoll.Interval = RampRoll.Interval 'reset timer
			exit Sub
		End If
		if x = uBound(RampBalls) then 	'debug
			Debug.print "WireRampOn error, ball queue is full: " & vbnewline & _
			RampBalls(0, 0) & vbnewline & _
			Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & "type:" & RampType(1) & vbnewline & _
			Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & "type:" & RampType(2) & vbnewline & _
			Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & "type:" & RampType(3) & vbnewline & _
			Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & "type:" & RampType(4) & vbnewline & _
			Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & "type:" & RampType(5) & vbnewline & _
			" "
		End If
	next
End Sub

' WRemoveBall (BallId)
'    Description: This subroutine is called from the RampRollUpdate subroutine 
'                 and is used to remove and stop the ball rolling sounds
Sub WRemoveBall(ID)		'Remove ball
	'Debug.Print "In WRemoveBall() + Remove ball from loop array"
	dim ballcount : ballcount = 0
	dim x : for x = 1 to Ubound(RampBalls)
		if ID = RampBalls(x, 1) then 'remove ball
			Set RampBalls(x, 0) = Nothing
			RampBalls(x, 1) = Empty
			RampType(x) = Empty
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end If
		'if RampBalls(x,1) = Not IsEmpty(Rampballs(x,1) then ballcount = ballcount + 1
		if not IsEmpty(Rampballs(x,1)) then ballcount = ballcount + 1
	next
	if BallCount = 0 then RampBalls(0,0) = False	'if no balls in queue, disable timer update
End Sub

Sub RampRoll_Timer():RampRollUpdate:End Sub

Sub RampRollUpdate()		'Timer update
	dim x : for x = 1 to uBound(RampBalls)
		if Not IsEmpty(RampBalls(x,1) ) then 
			if BallVel(RampBalls(x,0) ) > 1 then ' if ball is moving, play rolling sound
				If RampType(x) then 
					PlaySound("RampLoop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitchV(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))				
					StopSound("wireloop" & x)
				Else
					StopSound("RampLoop" & x)
					PlaySound("wireloop" & x), -1, VolPlayfieldRoll(RampBalls(x,0)) * RampRollVolume * VolumeDial, AudioPan(RampBalls(x,0)), 0, BallPitch(RampBalls(x,0)), 1, 0, AudioFade(RampBalls(x,0))
				End If
				RampBalls(x, 2)	= RampBalls(x, 2) + 1
			Else
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
			end if
			if RampBalls(x,0).Z < 30 and RampBalls(x, 2) > RampMinLoops then	'if ball is on the PF, remove  it
				StopSound("RampLoop" & x)
				StopSound("wireloop" & x)
				Wremoveball RampBalls(x,1)
			End If
		Else
			StopSound("RampLoop" & x)
			StopSound("wireloop" & x)
		end if
	next
	if not RampBalls(0,0) then RampRoll.enabled = 0

End Sub

' This can be used to debug the Ramp Roll time.  You need to enable the tbWR timer on the TextBox
Sub tbWR_Timer()	'debug textbox
	me.text =	"on? " & RampBalls(0, 0) & " timer: " & RampRoll.Enabled & vbnewline & _
	"1 " & Typename(RampBalls(1, 0)) & " ID:" & RampBalls(1, 1) & " type:" & RampType(1) & " Loops:" & RampBalls(1, 2) & vbnewline & _
	"2 " & Typename(RampBalls(2, 0)) & " ID:" & RampBalls(2, 1) & " type:" & RampType(2) & " Loops:" & RampBalls(2, 2) & vbnewline & _
	"3 " & Typename(RampBalls(3, 0)) & " ID:" & RampBalls(3, 1) & " type:" & RampType(3) & " Loops:" & RampBalls(3, 2) & vbnewline & _
	"4 " & Typename(RampBalls(4, 0)) & " ID:" & RampBalls(4, 1) & " type:" & RampType(4) & " Loops:" & RampBalls(4, 2) & vbnewline & _
	"5 " & Typename(RampBalls(5, 0)) & " ID:" & RampBalls(5, 1) & " type:" & RampType(5) & " Loops:" & RampBalls(5, 2) & vbnewline & _
	"6 " & Typename(RampBalls(6, 0)) & " ID:" & RampBalls(6, 1) & " type:" & RampType(6) & " Loops:" & RampBalls(6, 2) & vbnewline & _
	" "
End Sub


Function BallPitch(ball) ' Calculates the pitch of the sound based on the ball speed
    BallPitch = pSlope(BallVel(ball), 1, -1000, 60, 10000)
End Function

Function BallPitchV(ball) ' Calculates the pitch of the sound based on the ball speed Variation
	BallPitchV = pSlope(BallVel(ball), 1, -4000, 60, 7000)
End Function


'Ramp triggers

Sub RampStart1_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart2_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart3_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart4_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub RampStart5_hit
        WireRampOn True           ' true means plastic ramp here
End Sub

Sub Switch2Flat1_hit
    WireRampOff
        WireRampOn True          ' flat
End Sub

Sub Switch2Flat2_hit
    WireRampOff
        WireRampOn True          ' flat
End Sub

Sub Switch3Flat3_hit
    WireRampOff
        WireRampOn True          ' flat
End Sub


Sub DropOnRamp_hit
 PLaysoundat "dropOnramp", leftflipper001
End Sub

Sub Switch2Wire1_Hit
    If ActiveBall.VelY < -0.5 Then
        WireRampOff                 ' stop the flat/normal ramp roll that was started earlier
        WireRampOn False            ' wire mode
    ElseIf ActiveBall.VelY > 0.5 Then
        WireRampOff                 ' stop the wire ramp roll started at this trigger
        WireRampOn True             ' flat mode
    End If
End Sub

Sub Switch2Wire2_Hit
        WireRampOff                 ' stop the flat/normal ramp roll that was started earlier
        WireRampOn False            ' wire mode
End Sub

Sub Switch2Wire3_Hit
    If ActiveBall.VelY < -0.5 Then
        WireRampOff                 ' stop the flat/normal ramp roll that was started earlier
        WireRampOn False            ' wire mode
    ElseIf ActiveBall.VelY > 0.5 Then
        WireRampOff                 ' stop the wire ramp roll started at this trigger
        WireRampOn True             ' flat mode
    End If
End Sub

Sub Switch3Wire1_Hit
    If ActiveBall.VelY < -0.5 Then
        WireRampOff                 ' stop the flat/normal ramp roll that was started earlier
        WireRampOn False            ' wire mode
    ElseIf ActiveBall.VelY > 0.5 Then
        WireRampOff                 ' stop the wire ramp roll started at this trigger
        WireRampOn True             ' flat mode
    End If
End Sub

Sub RandomSoundRampStop(obj)
	Select Case Int(rnd*3)
		Case 0: PlaySoundAtVol "wireramp_stop1", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_1"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 1: PlaySoundAtVol "wireramp_stop2", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_Strong_2"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
		Case 2: PlaySoundAtVol "wireramp_stop3", obj, 0.02*volumedial:PlaySoundAtLevelActiveBall ("Rubber_1_Hard"), Vol(ActiveBall) * RubberStrongSoundFactor * 0.6
	End Select
End Sub

'******************************************************
'**** END RAMP ROLLING SFX
'******************************************************

'******************************************************
'  Fleep  Supporting Ball & Sound Functions
'******************************************************

Function AudioFade(tableobj) ' Fades between front and back of the table (for surround systems or 2x2 speakers, etc), depending on the Y position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.y * 2 / tableheight - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioFade = CSng(tmp ^ 10)
	Else
		AudioFade = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function AudioPan(tableobj) ' Calculates the pan for a tableobj based on the X position on the table. "table1" is the name of the table
	Dim tmp
	tmp = tableobj.x * 2 / tablewidth - 1
	
	If tmp > 7000 Then
		tmp = 7000
	ElseIf tmp <  - 7000 Then
		tmp =  - 7000
	End If
	
	If tmp > 0 Then
		AudioPan = CSng(tmp ^ 10)
	Else
		AudioPan = CSng( - (( - tmp) ^ 10) )
	End If
End Function

Function Vol(ball) ' Calculates the volume of the sound based on the ball speed
	Vol = CSng(BallVel(ball) ^ 2)
End Function

Function Volz(ball) ' Calculates the volume of the sound based on the ball speed
	Volz = CSng((ball.velz) ^ 2)
End Function

Function Pitch(ball) ' Calculates the pitch of the sound based on the ball speed
	Pitch = BallVel(ball) * 20
End Function

Function BallVel(ball) 'Calculates the ball speed
	BallVel = Int(Sqr((ball.VelX ^ 2) + (ball.VelY ^ 2) ) )
End Function

Function VolPlayfieldRoll(ball) ' Calculates the roll volume of the sound based on the ball speed
	VolPlayfieldRoll = RollingSoundFactor * 0.0005 * CSng(BallVel(ball) ^ 3)
End Function

Function PitchPlayfieldRoll(ball) ' Calculates the roll pitch of the sound based on the ball speed
	PitchPlayfieldRoll = BallVel(ball) ^ 2 * 15
End Function

Function RndInt(min, max) ' Sets a random number integer between min and max
	RndInt = Int(Rnd() * (max - min + 1) + min)
End Function

Function RndNum(min, max) ' Sets a random number between min and max
	RndNum = Rnd() * (max - min) + min
End Function

'********************* START OF PUPDMD FRAMEWORK v3.0 BETA *************************
'******************************************************************************
'*****   Create a PUPPack within PUPPackEditor for layout config!!!  **********
'******************************************************************************
'
'  Quick Steps:
'      1>  create a folder in PUPVideos with Starter_PuPPack.zip and call the folder "yourgame"
'      2>  above set global variable pGameName="yourgame"
'      3>  copy paste the settings section above to top of table script for user changes.
'      4>  on Table you need to create ONE timer only called pupDMDUpdate and set it to 250 ms enabled on startup.
'      5>  go to your table1_init or table first startup function and call PUPINIT function
'      6>  Go to bottom on framework here and setup game to call the appropriate events like pStartGame (call that in your game code where needed)...etc
'      7>  attractmodenext at bottom is setup for you already,  just go to each case and add/remove as many as you want and setup the messages to show.  
'      8>  Have fun and use pDMDDisplay(xxxx)  sub all over where needed.  remember its best to make a bunch of mp4 with text animations... looks the best for sure!
'
'
'Note:  for *Future Pinball* "pupDMDupdate_Timer()" timer needs to be renamed to "pupDMDupdate_expired()"  and then all is good.
'       and for future pinball you need to add the follow lines near top
'Need to use BAM and have com idll enabled.
'				Dim icom : Set icom = xBAM.Get("icom") ' "icom" is name of "icom.dll" in BAM\Plugins dir
'				if icom is Nothing then MSGBOX "Error cannot run without icom.dll plugin"
'				Function CreateObject(className)       
'   					Set CreateObject = icom.CreateObject(className)   
'				End Function


'**************************
'   PinUp Player USER Config
'**************************

Dim pGameName       : pGameName="Aladdin"  'pupvideos foldername, probably set to cGameName in realworld


Const HasPuP = True   'dont set to false as it will break pup

Const pTopper=0
Const pBackglass=2
Const pDMD=5
Const pMusic=4
Const pCallouts=6
Const pMusic2=7
Const pMusic3=8
Const pPopUP=11
Const pPopUP2=12


'pages
Const pDMDBlank=0
Const pScores=1
Const pBigLine=2
Const pThreeLines=3
Const pTwoLines=4
Const pTargerLetters=5

dim clRed:   clRed = rgb(249,21,48)
dim clGreen: clGreen = rgb(83,189,36)
dim clBlue:  clBlue = rgb(1,196,252)
dim clWhite: clWhite = rgb(255,255,255)
dim clNavy:  clNavy = rgb(34,32,72)
dim clBlack:  clNavy = rgb(0,0,0)
dim clGold:  clGold = rgb(168,168,0)
dim clPinkDark:  clPinkDark = rgb(128,0,255)
dim clViolet:  clViolet = rgb(128,0,128)
dim clPink:  clPink = rgb(255,128,192)


Dim PuPlayer
dim PUPDMDObject  'for realtime mirroring.
Dim pDMDlastchk: pDMDLastchk= -1    'performance of updates
Dim pDMDCurPage: pDMDCurPage= 0     'default page is empty.
Dim pInAttract : pInAttract=false   'pAttract mode
Dim pFrameSizeX: pFrameSizeX=1920     'DO NOT CHANGE, this is pupdmd author framesize
Dim pFrameSizeY: pFrameSizeY=1080     'DO NOT CHANGE, this is pupdmd author framesize
Dim pUseFramePos : pUseFramePos=1     'DO NOT CHANGE, this is pupdmd author setting


'*************  starts PUP system,  must be called AFTER b2s/controller running so put in last line of table1_init
Sub PuPInit

Set PuPlayer = CreateObject("PinUpPlayer.PinDisplay")   
PuPlayer.B2SInit "", pGameName
PuPlayer.LabelInit pDMD
pSetPageLayouts
pDMDSetPage(pDMDBlank)   'set blank text overlay page.
pDMDStartUP				 ' firsttime running for like an startup video..
End Sub 'end PUPINIT

Sub pDMDStartUP
'do stuff fancy pants on first run
'pInAttract = True
'pDMDSetPage(pScores)
	pAttractStart
end Sub 'end DMDStartup

Sub pTranslatePos(Byref xpos, byref ypos)  'if using uUseFramePos then all coordinates are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateY(Byref ypos)           'if using uUseFramePos then all heights are based on framesize
   ypos=int(ypos/pFrameSizeY*10000) / 100
end Sub

Sub pTranslateX(Byref xpos)           'if using uUseFramePos then all heights are based on framesize
   xpos=int(xpos/pFrameSizeX*10000) / 100
end Sub



'***********************************************************PinUP Player DMD Helper Functions

Sub pDMDLabelSet(labName,LabText)
PuPlayer.LabelSet pDMD,labName,LabText,1,""   
end sub


Sub pDMDLabelHide(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",0,""   
end sub

Sub pDMDLabelShow(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,""   
end sub

Sub pDMDLabelVisible(labName, isVis)
PuPlayer.LabelSet pDMD,labName,"`u`",isVis,""   
end sub

Sub pDMDLabelSendToBack(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zback': 1 }"   
end sub

Sub pDMDLabelSendToFront(labName)
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'ztop': 1 }"   
end sub

sub pDMDLabelSetPos(labName, byVal xpos, byVal ypos)
   if pUseFramePos=1 Then pTranslatePos xpos,ypos
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xpos':"&xpos& ",'ypos':"&ypos&"}"    
end sub

sub pDMDLabelSetSizeImage(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'width':"& lWidth & ",'height':"&lHeight&"}" 
end sub

sub pDMDLabelSetSizeText(labName, byVal fHeight)
   if pUseFramePos=1 Then pTranslateY fHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'size':"&fHeight&"}" 
end sub

sub pDMDLabelSetAutoSize(labName, byVal lWidth, byVal lHeight)
   if pUseFramePos=1 Then pTranslatePos lWidth,lHeight
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'autow':"& lWidth & ",'autoh':"&lHeight&"}" 
end sub

sub PDMDLabelSetAlign(labName,xAlign, YAlign)  '0=left 1=center 2=right,  note you should use center as much as possible because some things like rotate/zoom/etc only look correct with center align!
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'xalign':"& xAlign & ",'yalign':"&yAlign&"}"     
end sub

sub pDMDLabelStopAnis(labName)    'stop any pup animations on label/image (zoom/flash/pulse).  this is not about animated gifs
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'stopani':1 }" 
end sub

sub pDMDLabelSetRotateText(labName, fAngle)  ' in tenths.  so 900 is 90 degrees.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetRotate(labName, fAngle)  ' in tenths.  so 900 is 90 degrees. rotate support for images too.  note images must be aligned center to rotate properly(default)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'rotate':"&fAngle&"}" 
end sub

sub pDMDLabelSetZoom(labName, fFactor)  ' fFactor is 120 for 120% of current height, 80% etc...
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'zoom':"&fFactor&"}" 
end sub

sub pDMDLabelSetColor(labName, lCol)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&lCol&"}" 
end sub

sub pDMDLabelSetAlpha(labName, lAlpha)  '0-255  255=full, 0=blank
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'alpha':"&lAlpha&"}" 
end sub

sub pDMDLabelSetColorGradient(labName, byVal startCol, byVal EndCol)
dim GS: GS=1
if startCol=EndCol Then GS=0  'turn grad off is same colors.
PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,'gradstate':"&GS&" , 'gradcolor':"&endCol&"}" 
end sub

sub pDMDLabelSetColorGradientPercent(labName, byVal startCol, byVal EndCol, byVal StartPercent)
if startCol=EndCol Then StartPercent=0  'turn grad off is same colors.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'color':"&startCol&" ,  'gradstate':"&StartPercent&", 'gradcolor':"&endCol&"}" 
end sub

sub pDMDLabelSetGrayScale(labName, isGray)  'only on image objects.  will show as grayscale.  1=gray filter on 0=off normal mode
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'grayscale':"&isGray&"}" 
end sub

sub pDMDLabelSetFilter(labName, fMode)  ''fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear),blur)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'filter':"&fmode&"}" 
end sub

Sub pDMDLabelFlashFilter(LabName,byVal timeSec,fMode)   'timeSec in ms  'fmode 1-5 (invertRGB, invert,grayscale,invertalpha,clear,blur)
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':9,'fq':150,'len':" & (timeSec) & ",'fm':" & fMode & "}"   
end sub



sub pDMDLabelSetShadow(labName,lCol,offsetx,offsety,isVis)  ' shadow of text
dim ST: ST=1 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub

sub pDMDLabelSetBorder(labName,lCol,offsetx,offsety,isVis)   'outline/border around text.
dim ST: ST=2 : if isVIS=false Then St=0
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowtype': "&ST&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&"}"
end sub



'animations   'pDMDLabelPulseText "pulsetext","jackpot",4000,rgb(100,0,0)

sub pDMDLabelPulseText(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub
sub pDMDLabelPulseText2(LabName,LabValue,mLen,mColor)       'mlen in ms
    PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumber(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0 }"    
end Sub

sub pDMDLabelPulseImage(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':80,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }"
end Sub
sub pDMDLabelPulseImage2(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
	PuPlayer.LabelSet pDMD, LabName, "`u`", isVis, "{'mt':1, 'at':2, 'fsstart':10, 'fsend':100, 'yps':0, 'ype':-10, 'len':" & (mLen) & "}" 
'	PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':10,'hend':120,'len':" & (mLen) & ",'pspeed': 0 }" 
end Sub
sub pDMDLabelPulseImage3(LabName,mLen,isVis)       'mlen in ms isVis is state after animation
	PuPlayer.LabelSet pDMD, LabName, "`u`", isVis, "{'mt':1,'at':4,'hstart':100,'hend':250,'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelPulseTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  same subs as above but youspecifiy zoom start and zoom end in % height of original font.
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelPulseNumberEX(LabName,LabValue,mLen,mColor,pNumStart,pNumEnd,pNumformat,isVis,zStart,zEnd)   'pnumformat 0 no format, 1 with thousands  mLen=ms
     PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0,'fc':" & mColor & ",'numstart':"&pNumStart&",'numend' :"&pNumEnd&", 'numformat':"&pNumFormat&",'aa':0}"    
end Sub

sub pDMDLabelPulseImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms isVis is state after animation
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':4,'hstart':"&zStart&",'hend':"&zEnd&",'len':" & (mLen) & ",'pspeed': 0 }"
end Sub

sub pDMDLabelWiggleText(LabName,LabValue,mLen,mColor)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleTextEX(LabName,LabValue,mLen,mColor,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,LabValue,isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & mColor & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImage(LabName,mLen,isVis)         'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':-45,'rend':45,'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub

sub pDMDLabelWiggleImageEX(LabName,mLen,isVis,zStart,zEnd)       'mlen in ms  zstart MUST be less than zEND.  -40 to 40 for example
    PuPlayer.LabelSet pDMD,labName,"`u`",isVis,"{'mt':1,'at':8,'rstart':"&zStart&",'rend':"&zEnd&",'len':" & (mLen) & ",'rspeed': 5,'fc':" & 0 & ",'aa':0 }"
end Sub




sub pDMDPNGAnimate(labName,cSpeed)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&"}" 
end sub

sub pDMDPNGAnimateEx(labName,startFrame,endFrame,LoopMode)  'sets up the apng/gif settings before you call animate.  if you set start/end frame same if will display that frame, set start to -1 to reset settings.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&startFrame&",'gifend':"&endFrame&",'gifloop':"&loopMode&" }"          'gifstart':3, 'gifend':10, 'gifloop': 1
end sub

sub pDMDPNGShowFrame(labName,fFrame)  'in a animated png/gif, will set it to an individual frame so you could use as an imagelist control
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'gifstart':"&fFrame&",'gifend':"&fFrame&" }"          '
end sub

sub pDMDPNGAnimateOnce(labName,cSpeed)  'will show an animated gif/png and then hide when done, overrides loop to force stop at end.
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':"&cSpeed&", 'gifloop': 0 , 'aniendhide':1 }" 
end sub

sub pDMDPNGAnimateReset(labName)  'speed is frame timer, 0 = stop animation  100 is 10fps for animated png and gif nextframe timer, this will show anigif and hide at end no loop
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'animate':0, 'gifloop': 1 , 'aniendhide':0 , 'gifstart':-1}" 
end sub





sub pDMDLabelSetOutShadow(labName, lCol,offsetx,offsety,isOutline,isVis)
   PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'shadowcolor':"&lCol&",'shadowstate': "&isVis&", 'xoffset': "&offsetx&", 'yoffset': "&offsety&", 'outline': "&isOutline&"}"
end sub

sub pDMDLabelMoveHorz(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off    or can use % 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveVert(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off   or can use %  
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2,'ad':1 }"    
end Sub

sub pDMDLabelMoveTO(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,1,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':2 ,'ad':1}"    
end Sub

sub pDMDLabelMoveHorzFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off, or can use %
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pMoveStart&",'xpe' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveVertFade(LabName,LabValue,mLen,mColor,pMoveStart,pMoveEnd)   'pmovestart is -1= left-off 0=current pos 1=right-off  or can use %   
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'yps':"&pMoveStart&",'ype' :"&pMoveEnd&", 'tt':2 ,'ad':1, 'af':700}"    
end Sub

sub pDMDLabelMoveTOFade(LabName,LabValue,mLen,mColor,byVal pStartX,byVal pStartY,byVal pEndX,byVal pEndY)   'pmovestart is -1= left-off 0=current pos 1=right-off
     if pUseFramePos=1 AND (pStartX+pStartY+pEndx+pendY)>4 Then 
                       pTranslatePos pStartX,pStartY
                       pTranslatePos pEndX,pEndY
     end IF 
     PuPlayer.LabelSet pDMD,labName,LabValue,0,"{'mt':1,'at':2, 'len':" & (mLen) & ", 'fc':" & mColor & ",'xps':"&pStartX&",'xpe' :"&pEndX& ",'yps':"&pStartY&",'ype' :"&pEndY&", 'tt':6 ,'ad':1, 'af':700}"    
end Sub





sub pDMDLabelFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':5,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDLabelFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':5,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub


sub pDMDLabelFadePulse(LabName,mLen,mColor)   'alpha is 255 max, 0=clear. alpha start/end and pulsespeed of change
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':6,'astart':70,'aend':255,'len':" & (mLen) & ",'pspeed': 40,'fc':" & mColor & "}" 
end Sub

Sub pDMDLabelFlash(LabName,byVal timeSec, mColor)   'timeSec in ms
    if timeSec<20 Then timeSec=timeSec*1000
    PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec) & ",'fc':" & mColor & "}"   
end sub



sub pDMDScreenFadeOut(LabName,mLen)   'alpha is 255 max, 0=clear.  
     PuPlayer.LabelSet pDMD,labName,"`u`",0,"{'mt':1,'at':7,'astart':255,'aend':0,'len':" & (mLen) & " }"    
end Sub

sub pDMDScreenFadeIn(LabName,mLen)    'alpha is 255 max, 0=clear. 
     PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':1,'at':7,'astart':0,'aend':255,'len':" & (mLen) & " }"    
end Sub



Sub pDMDScrollBig(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'xps':1,'xpe':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*1) & ",'tt':0,'fc':" & mColor & "}"
end sub

Sub pDMDScrollBigV(LabName,msgText,byVal timeSec,mColor) 'timeSec in MS
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,0,"{'mt':1,'at':2,'yps':1,'ype':-1,'len':" & (timeSec) & ",'mlen':" & (timeSec*0.8) & ",'tt':0,'fc':" & mColor & "}"
end sub


Sub pDMDZoomBig(LabName,msgText,byVal timeSec,mColor,isVis,byVal zStart,byVal zEnd)  'timeSec in MS  zstart/end is % of screen height  notice aa antialias is 0 for big font zooms for performance.  'ns is size by %label height.
if timeSec<20 Then timeSec=timeSec*1000
PuPlayer.LabelSet pDMD,LabName,msgText,isVis,"{'mt':1,'at':3,'hstart':" & (zStart) & ",'hend':" & (zEnd) & ",'len':" & (timeSec) & ",'mlen':" & (timeSec*0.4) & ",'tt':" & 0 & ",'fc':" & mColor & ", 'ns':1, 'aa':0}"
end sub




Sub AudioDuckPuP(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" }"             
end Sub

Sub AudioDuckPuPAll(MasterPuPID,VolLevel)  
'will temporary volume duck all pups (not masterid) till masterid currently playing video ends.  will auto-return all pups to normal.
'VolLevel is number,  0 to mute 99 for 99%  
PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& MasterPuPID& ", ""FN"": 42, ""DV"": "&VolLevel&" , ""ALL"":1 }"             
end Sub




Sub pSetAspectRatio(PuPID, arWidth, arHeight)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 50, ""WIDTH"": "&arWidth&", ""HEIGHT"": "&arHeight&" }"   
end Sub  

Sub pDisableLoopRefresh(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 2, ""FF"":0, ""FO"":0 }"   
end Sub  

'set safeloop mode on current playing media.  Good for background videos that refresh often?  { "mt":301, "SN": XX, "FN":41 }
Sub pSafeLoopModeCurrentVideo(PuPID)
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "&PuPID& ", ""FN"": 41 }"   
end Sub  

Sub pSetLowQualityPc  'sets fulldmd to run in lower quality mode (slowpc mode)  AAlevel for text is removed and other performance/quality items.  default is always run quality, 
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":45, ""SP"":1 }"    'slow pc mode
end Sub 

Sub pDMDLabelDispose(labName)   'not needed unless you want to want to free a heavy resource label from cache/memory.  or temp lables that you created.  performance reasons.
      PuPlayer.LabelSet pDMD,labName,"`u`",1,"{'mt':2,'dispose': 1 }"   
end Sub

Sub pDMDAlwaysPAD  'will pad all text with a space before and after to help with possible text clipping.
     PuPlayer.SendMSG "{ ""mt"":301, ""SN"": 5, ""FN"":46, ""PA"":1 }"    'slow pc mode
end Sub   


Sub pDMDSetHUD(isVis)   'show hide just the pBackGround object (HUD overlay).      
    pDMDLabelVisible "pBackGround",isVis
end Sub  




Sub pDMDSetPage(pagenum)    
    PuPlayer.LabelShowPage pDMD,pagenum,0,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub

Sub pDMDSplashPage(pagenum, cTime)    'cTime is seconds.  3 5,  it will auto return to current page after ctime
    PuPlayer.LabelShowPage pDMD,pagenum,cTime,""   'set page to blank 0 page if want off
    PDMDCurPage=pagenum
end Sub



Sub PDMDSplashPagePlaying(pagenum)  'will hide HUD and show labepage while current media is playing. and then autoreturn.
    PuPlayer.LabelShowPage pDMD,pagenum,500,"hidehudplay"
end Sub    

Sub PDMDSplashPagePlayingHUD(pagenum)  'will show labelpage and auto return to def after current video stopped
    PuPlayer.LabelShowPage pDMD,pagenum,500,"returnplay"
end Sub    


Sub pHideOverlayDuringCurrentPlay() 'will hide pup text labels and HUD till current video stops playing.
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 34 }"             'hideoverlay text during next videoplay on DMD auto return
end Sub


Sub pSetVideoPosMS(mPOS)  'set position of video/audio in ms,  must be playing already or will be ignored.  { "mt":301, "SN": XX, "FN":51, "SP": 3431} 
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ", ""FN"": 51, ""SP"":"&mPOS&" }"
end Sub

sub pAllVisible(lvis)   '0/1 to show hide pup text overlay and HUD
    PuPlayer.SendMSG "{ ""mt"":301, ""SN"": "& "5"& ",""OT"":"&lvis&", ""FN"": 3 }"             'hideoverlay text force
end Sub


Sub pDMDSetBackFrame(fname)
  PuPlayer.playlistplayex pDMD,"PUPFrames",fname,0,1    
end Sub

Sub pDMDBackLoopStart(fPlayList,fname)
  PuPlayer.playlistplayex pDMD,fPlayList,fname,0,1
  PuPlayer.SetBackGround pDMD,1
end Sub

Sub pDMDBackLoopStop
  PuPlayer.SetBackGround pDMD,0
  PuPlayer.playstop pDMD
end Sub

'jukebox mode will auto advance to next media in playlist and you can use next/prior sub to manuall advance
'you should really have a specific pupid# display like musictrack that is only used for the playlist
'sub PUPDisplayAsJukebox(pupid) needs to be called/set prior to sending your first media to that pupdisplay.
'pupid=pupdiplay# like pMusic

Sub PUPDisplayAsJukebox(pupid)
PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':30, 'PM':1 }")
End Sub

Sub PuPlayListPrior(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':1 }")
End Sub

Sub PuPlayListNext(pupid)
 PuPlayer.SendMSG("{'mt':301, 'SN': " & pupid & ", 'FN':31, 'PM':2 }")
End Sub

Sub pDMDPause()
 PuPlayer.playpause pDMD
end Sub

Sub pDMDResume()
 PuPlayer.playresume pDMD
end Sub

Sub pDMDStop()
 PuPlayer.playstop pDMD
end Sub

Sub pDMDVolumeDef(cVol)  'sets the default volume of player, doesnt affect current playing media
 PuPlayer.setVolume pdmd,cVol
end Sub

Sub pDMDVolumeCurrent(cVol)  'sets the volume of current media (like to duck audio), doesnt affect default volume for next media.
 PuPlayer.setVolumeCurrent pdmd,cVol
end Sub

Sub pDMDSetLoop(isLoop)     'it will loop the currently playing file 0=cancel looping 1=loop
 PuPlayer.setLoop pDMD,isLoop
end Sub

Sub pDMDBackground(isBack)  'will set the currently playing file as background video and continue to loop and return to it automatically 0=turn off as background.
 PuPlayer.setBackground pDMD,isBack
end Sub


Sub PuPEvent(EventNum)
if hasPUP=false then Exit Sub
PuPlayer.B2SData "E"&EventNum,1  'send event to puppack driver  
End Sub


Sub pupCreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos,pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,lFont,lSize,lColor,0,1,1,1,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslateY lSize
PuPlayer.LabelSet pDMD,lName,lValue,lvis,"{'mt':2,'xpos':"& xpos & ",'ypos':"&ypos&",'fonth':"&lsize&",'v2':1 }"
end Sub

Sub pupCreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
PuPlayer.LabelNew pDMD,lName ,"",50,RGB(100,100,100),0,1,1,0,1,pagenum,lvis
if pUseFramePos=1 Then pTranslatePos xpos,ypos
if pUseFramePos=1 Then pTranslatePos Iwidth,iHeight
PuPlayer.LabelSet pDMD,lName,lFilename,lvis,"{'mt':2,'width':"&IWidth&",'height':"&Iheight&",'xpos':"&xpos&",'ypos':"&ypos&",'v2':1 }"
end Sub


Sub pSetPageLayouts

DIM dmddef
DIM dmdalt
DIM dmdscr
DIM dmdfixed
DIM dmdtomb
DIM dmdaladd

'Using FULL BIG LCD PuPDMD  ************ lcd **************
'
'Sub CreateLabel(lName, lValue, lFont, lSize, lColor, xpos, ypos, alignX, alignY, pagenum, lvis)
'
'Sub CreateLabelImage(lName, lFilename,xpos, ypos, Iwidth, Iheight, pagenum, lvis)
	
		dmdalt="PKMN Pinball"
		dmdfixed="Instruction"
		dmdscr="Impact"    'main scorefont
		dmddef="Zig"
		dmdtomb="Ruben"
		dmdaladd="Aladdin"

	'Page 1 (default score display)
		'PuPlayer.LabelNew pDMD,"Credits" ,dmddef,20,33023   ,0,2,2,95,0,1,0
       
        pDMDAlwaysPAD		'we pad all text with space before and after for shadow clipping/etc
'1920,1080
		pupCreateLabelImage "Overlay", "PuPOverlays\\AladOverlayVide.png",960,540,1920,1080,1,0
'		pupCreateLabel "Credits","",      dmdaladd, 110,  5548032	,470,385,1,0
'		pupCreateLabel "Ball","",         dmdaladd, 110,  12870144	,1440,385,1,0
		pupCreateLabel "Credits","",      dmdaladd, 110,  5548032	,1300,55,1,0
		pupCreateLabel "Ball","",         dmdaladd, 110,  12870144	,1765,55,1,0
		pupCreateLabel "Play1","",        dmdaladd, 170, 5112227	,500,65,1,0
		pupCreateLabel "CurScore","",     dmdscr, 170, 32896	,960,190,1,0 'ok	
		pupCreateLabel "CScoreP1","",     dmdscr, 60,  32896	,441,300,1,0
		pupCreateLabel "CScoreP2","",     dmdscr, 60,  12870144	,787,300,1,0
		pupCreateLabel "CScoreP3","",     dmdscr, 60,  5112227		,1132,300,1,0
		pupCreateLabel "CScoreP4","",     dmdscr, 60,  1992703,  1478,300,1,0
		pupCreateLabel "JackpotValue1","",       dmdaladd, 110,  clWhite	,960,380,1,0
		pupCreateLabel "JackpotValue2","",       dmdaladd, 90,  clWhite	,960,450,1,0
'		PuPlayer.LabelNew pDMD,"pupStatus",dmdscr,16,32896   ,0,1,1,50,60,1,0 'OK
		pDMDLabelSetBorder "CurScore",clBlack,2,2,1
		pDMDLabelSetShadow "CurScore",RGB(0,0,0),3,3,1
		pDMDLabelSetBorder "Ball",clBlack,2,2,1
		pDMDLabelSetBorder "Credits",clBlack,2,2,1
		pDMDLabelSetBorder "JackpotValue1",clBlack,2,2,1
		pDMDLabelSetBorder "JackpotValue2",clBlack,2,2,1
		pDMDLabelSetBorder "Play1",clBlack,2,2,1
		pDMDLabelSetShadow "Play1",RGB(0,0,0),3,3,1

		pupCreateLabel "Bumper001","",  dmdaladd, 170,  			16777215,  1200,  450, 1, 1
		pupCreateLabel "Bumper002","",  dmdaladd, 170,  			16777215,  820,  550, 1, 1
		pupCreateLabel "Bumper003","",  dmdaladd, 170,  			16777215,  1080,  650, 1, 1
		pDMDLabelSetBorder "Bumper001",clBlack,2,2,1
		pDMDLabelSetBorder "Bumper002",clBlack,2,2,1
		pDMDLabelSetBorder "Bumper003",clBlack,2,2,1

		pupCreateLabelImage "Miss1", "PuPAlphas\\MissionHarem001.png",960,540,1920,1080,1,0
		pupCreateLabelImage "Miss2", "PuPAlphas\\MissionHarem002.png",960,540,1920,1080,1,0
		pupCreateLabelImage "Miss3", "PuPAlphas\\MissionHarem003.png",960,540,1920,1080,1,0
		'(Text 1 Line)
		pupCreateLabel "Texte1a","",    dmdaladd, 250, 16732584, 960,  680, 1, 0 'Center 
		'(Text 2 Line)
		pupCreateLabel "Texte2a","",    dmdaladd, 210, 16732584, 960,  550, 1, 0
		pupCreateLabel "Texte2b","",    dmdaladd, 200, 5112227,  960,  720, 1, 2
		'(Text 3 Line)
		pupCreateLabel "Texte3a","",    dmdaladd, 190,  5112227,  960,  540, 1, 0
		pupCreateLabel "Texte3b","",    dmdaladd, 180,  16732584, 960,  680, 1, 0
		pupCreateLabel "Texte3c","",    dmdaladd, 190,  5112227,  960,  820, 1, 0
		pDMDLabelSetBorder "Texte1a",clBlack,2,2,1
		pDMDLabelSetBorder "Texte2a",clBlack,2,2,1
		pDMDLabelSetBorder "Texte2b",clBlack,2,2,1
		pDMDLabelSetBorder "Texte3a",clBlack,2,2,1
		pDMDLabelSetBorder "Texte3b",clBlack,2,2,1
		pDMDLabelSetBorder "Texte3c",clBlack,2,2,1
	
		pupCreateLabel "HighScore","",   dmdscr, 110,  436985,    822,  880, 1, 1
		pupCreateLabel "HighScoreL1","", dmdscr, 110,  5112227, 1186, 880, 1, 1
		pupCreateLabel "HighScoreL2","", dmdscr, 110,  5112227, 1263, 880, 1, 1
		pupCreateLabel "HighScoreL3","", dmdscr, 110,  5112227,  1340, 880, 1, 1
		pDMDLabelSetBorder "HighScore",clBlack,2,2,1
		pDMDLabelSetShadow "HighScore",RGB(0,0,0),3,3,1
		pDMDLabelSetBorder "HighScoreL1",clBlack,2,2,1
		pDMDLabelSetBorder "HighScoreL2",clBlack,2,2,1
		pDMDLabelSetBorder "HighScoreL3",clBlack,2,2,1

		'Page 3 (default Attract)
		pupCreateLabel "AttractTitle", "", dmdaladd, 230, 16732584, 960, 400, 3, 0
		pupCreateLabel "AttractLine1", "", dmdaladd, 200, 5112227, 960, 580, 3, 0 
		pupCreateLabel "AttractLine2", "", dmdaladd, 200, 5112227, 960, 720, 3, 0
		pupCreateLabel "AttractLine3", "", dmdaladd, 200, 5112227, 960, 860, 3, 0
		pupCreateLabel "AttractLine4", "", dmdaladd, 300, 436985, 960, 170, 3, 0
		pupCreateLabel "AttractLine1Alt", "", dmdscr, 150, 5112227, 960, 580, 3, 0 
		pupCreateLabel "AttractLine2Alt", "", dmdscr, 150, 5112227, 960, 720, 3, 0
		pupCreateLabel "AttractLine3Alt", "", dmdscr, 150, 5112227, 960, 860, 3, 0
        pDMDLabelSetShadow "AttractTitle",RGB(0,0,0),3,3,1
		pDMDLabelSetBorder "AttractTitle",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine1",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine2",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine3",clBlack,2,2,1
		pDMDLabelSetBorder "AttractLine4",clBlack,2,2,1
		pDMDLabelSetBorder "AttractLine1Alt",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine2Alt",clBlack,2,2,1
        pDMDLabelSetBorder "AttractLine3Alt",clBlack,2,2,1


		'Bonus
		pupCreateLabel "Bonus1","",      dmdaladd, 120,  13264128, 	670,  520, 1, 1	'XP SlingCount Bonus
		pupCreateLabel "Bonus2","",      dmdaladd, 120,  55660,  	670,  620, 1, 1		'Rampes Bonus
		pupCreateLabel "Bonus3","",      dmdaladd, 120,  3739322, 	670,  720, 1, 1		'BumperCount (x20) Bonus
		pupCreateLabel "Bonus4","",      dmdaladd, 120,  25542,  	670,  820, 1, 1		'Mission Harem Bonus
		pupCreateLabel "BonusMultip","", dmdaladd, 150,  59624,    1400,  670, 1, 1
		pupCreateLabel "BonusTotal","",  dmdaladd, 160,  16777215,  960,  950, 1, 1
		pDMDLabelSetBorder "BonusMultip",clBlack,2,2,1
		pDMDLabelSetBorder "BonusTotal",clBlack,2,2,1

		'onverlay 
		PuPlayer.LabelNew pDMD,"Lock0",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Lock1",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Lock2",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Lock3",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabelImage "Lock1Anim", "PuPAlphas\\BallAnim.png",1440,385,150,150,1,0
		pupCreateLabelImage "Lock2Anim", "PuPAlphas\\BallAnim.png",1510,385,150,150,1,0
		PuPlayer.LabelNew pDMD,"Snake000",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Snake001",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Snake002",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Snake003",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Snake003b",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabelImage "Snake1Anim", "PuPAlphas\\SnakeAnim.png",449,385,100,100,1,0
		pupCreateLabelImage "Snake2Anim", "PuPAlphas\\SnakeAnim.png",540,385,100,100,1,0
		PuPlayer.LabelNew pDMD,"Pom001",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Pom002",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Pom003",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Pom004",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Pom005",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Pom006",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabel "PieceAdd","",  dmdaladd, 170,  			16777215,  160,  480, 1, 1
		pDMDLabelSetBorder "PieceAdd",clBlack,2,2,1
		PuPlayer.LabelNew pDMD,"Piece001",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Piece002",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Piece003",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Piece004",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Piece005",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Piece006",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		pupCreateLabelImage "CoeurAnim", "PuPAlphas\\CoeurAnim.png",320,700,100,100,1,0
		pupCreateLabel "CoeurAdd","",  dmdaladd, 170,  			16777215,  1820,  650, 1, 1
		pDMDLabelSetBorder "CoeurAdd",clBlack,2,2,1
		PuPlayer.LabelNew pDMD,"Coeur001",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
'		PuPlayer.LabelNew pDMD,"Coeur001", zoomfont, 			6, 16777215, 0,1,1, 50,50,1,0
		PuPlayer.LabelNew pDMD,"Coeur002",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Coeur003",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Coeur004",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Coeur005",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Coeur006",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"Coeur007",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0 'EXTRABALL
		PuPlayer.LabelNew pDMD,"WishGeni",zoomfont,				6,16777215 	,0,1,1, 0,0,1,0
		PuPlayer.LabelNew pDMD,"TapisMode",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
'		PuPlayer.LabelNew pDMD,"HaremMiss",zoomfont,			6,16777215 	,0,1,1, 0,0,1,0
'		pupCreateLabelImage "Harem007", "PuPAlphas\\Harem007.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem006", "PuPAlphas\\Harem006.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem005", "PuPAlphas\\Harem005.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem004", "PuPAlphas\\Harem004.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem003", "PuPAlphas\\Harem003.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem002", "PuPAlphas\\Harem002.png",960,540,1920,1080,1,0
'		pupCreateLabelImage "Harem001", "PuPAlphas\\Harem001.png",960,540,1920,1080,1,0
'1920,1080

'		pupCreateLabel "HaremMi1Time", "", dmdaladd, 90, 59624, 465, 840, 1, 0
'		pupCreateLabel "HaremMi2Time", "", dmdaladd, 90, 59624, 876, 875, 1, 0
'		pupCreateLabel "HaremMi3Time", "", dmdaladd, 90, 59624, 1325, 840, 1, 0
'		pupCreateLabel "HaremMi1Hit", "", dmdaladd, 90, 59624, 420, 905, 1, 0
'		pupCreateLabel "HaremMi2Hit", "", dmdaladd, 90, 59624, 825, 940, 1, 0
'		pupCreateLabel "HaremMi3Hit", "", dmdaladd, 90, 59624, 1278, 905, 1, 0
		pupCreateLabel "HaremMi1Hit", "", dmdaladd, 95,  59624, 555, 860, 1, 0
		pupCreateLabel "HaremMi2Hit", "", dmdaladd, 95,  59624, 850, 860, 1, 0
		pupCreateLabel "HaremMi3Hit", "", dmdaladd, 95,  59624, 1375, 860, 1, 0
		pupCreateLabel "HaremMi1Time", "", dmdaladd, 95, 59624, 555, 1020, 1, 0
		pupCreateLabel "HaremMi2Time", "", dmdaladd, 95, 59624, 850, 1020, 1, 0
		pupCreateLabel "HaremMi3Time", "", dmdaladd, 95, 59624, 1375, 1020, 1, 0

'		PuPlayer.LabelNew pDMD,"HaremMi1Expireda",dmdaladd,		12,32896   ,0,1,1,28,53,4,0 'pharem
'		PuPlayer.LabelNew pDMD,"HaremMi1Expiredb",dmdaladd,		12,32896   ,0,1,1,28,63,4,0 'pharem
'		PuPlayer.LabelNew pDMD,"HaremMi2Expireda",dmdaladd,		12,32896   ,0,1,1,50,61,4,0
'		PuPlayer.LabelNew pDMD,"HaremMi2Expiredb",dmdaladd,		12,32896   ,0,1,1,50,71,4,0
'		PuPlayer.LabelNew pDMD,"HaremMi3Expireda",dmdaladd,		12,32896   ,0,1,1,72,53,4,0
'		PuPlayer.LabelNew pDMD,"HaremMi3Expiredb",dmdaladd,		12,32896   ,0,1,1,72,63,4,0

		pupCreateLabel "HaremMi1Expireda", "", dmdaladd, 	110, 32896, 535, 575, 1, 0
		pupCreateLabel "HaremMi1Expiredb", "", dmdaladd, 	110, 32896, 535, 680, 1, 0
		pupCreateLabel "HaremMi2Expireda", "", dmdaladd, 	110, 32896, 960, 655, 1, 0
		pupCreateLabel "HaremMi2Expiredb", "", dmdaladd, 	110, 32896, 960, 765, 1, 0
		pupCreateLabel "HaremMi3Expireda", "", dmdaladd, 	110, 32896, 1380, 575, 1, 0
		pupCreateLabel "HaremMi3Expiredb", "", dmdaladd, 	110, 32896, 1380, 680, 1, 0
 
        'sample for image label
        'pupCreateLabelImage "cJewel","dmdmisc\\jewel.png",91.25,80.3,15,30,1,1
end Sub 'page Layouts



Sub pDMDSplashBig(msgText,timeSec, mColor)   'note timesec is seconds( 2, 3..etc) , if timesec>1000 then its ms. (2300, 3200)
PuPlayer.LabelShowPage pDMD,2,timeSec,""
PuPlayer.LabelSet pDMD,"Splash",msgText,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
end sub

Sub pDMDSplashScore(msgText,timeSec, mColor)   'note timesec is seconds( 2, 3..etc) , if timesec>1000 then its ms. (2300, 3200)
PuPlayer.LabelSet pDMD,"ScoreSplash",msgText,0,"{'mt':1,'at':1,'fq':150,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
end sub



'************************ called during gameplay to update Scores ***************************
Dim CurTestScore:CurTestScore=100000
Sub pUpdateScores  'call this ONLY on timer 300ms is good enough
if pDMDCurPage <> pScores then Exit Sub
	puPlayer.LabelSet pDMD,"CurScore","" & FormatNumber(PlayerScore(CurrentPlayer), 0),1,""
	puPlayer.LabelSet pDMD,"Play1","Player " & FormatNumber(CurrentPlayer, 0),1,""
	puPlayer.LabelSet pDMD,"Ball","Balls " & FormatNumber(BallinGame(CurrentPlayer), 0) & "/" & FormatNumber(BallsPerGame, 0),1,""
	'PuPlayer.LabelSet pDMD,"PlayersNumber","Players "& FormatNumber(PlayersPlayingGame, 0) & "/4",1,""
	PuPlayer.LabelSet pDMD,"Credits","Crédits "& Credits,1,""
	PuPlayer.LabelSet pDMD,"JackpotValue1","Jackpot value",1,""
	PuPlayer.LabelSet pDMD,"JackpotValue2",""& FormatNumber(JackpotValue(CurrentPlayer), 0),1,""

	If PlayersPlayingGame <= 1 or UseBGB2S = 1 Then 
		PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),0,""
		PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),0,""
		PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),0,""
		PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),0,""
	Else
		If PlayersPlayingGame > 1 Then PuPlayer.LabelSet pDMD,"CScoreP1","P1 - " & FormatNumber(PlayerScore(1),0),1,""
		If PlayersPlayingGame >= 2 Then PuPlayer.LabelSet pDMD,"CScoreP2","P2 - " & FormatNumber(PlayerScore(2),0),1,""
		If PlayersPlayingGame >= 3 Then PuPlayer.LabelSet pDMD,"CScoreP3","P3 - " & FormatNumber(PlayerScore(3),0),1,""
		If PlayersPlayingGame >= 4 Then PuPlayer.LabelSet pDMD,"CScoreP4","P4 - " & FormatNumber(PlayerScore(4),0),1,""
	End If
end Sub

PUPINIT   '*****REMINDER **********  you need to call PUPINIT sub in last line of table1_init



'*************************** SAMPLE END OF BALL BONUS SCREEN,  same idea/concept can be used for ATTRACT MODE too **************************************

'sub EndOfBallEnd
'pDMDSetPage 1 'put back to scores
'end Sub
'
'Sub EndOfBallNext
'    Select Case EOBCurIndex
'
'        Case 0
'            pDMDSplashLinesEOB "", FormatNumber(Bonus,0,, ,True) & " x " & BonusMult, 1, clWhite
'            EndOfBallBonus
'            TriggerScript 2200,"EndOfBallNext'"
'
'        Case 1
'            pDMDSplashLinesEOB "", FormatNumber(TotalBonusScore,0,, ,True), 1, clWhite
'            TriggerScript 2200,"EndOfBallNext'"
'
'        Case 2
'            EndOfBallEnd
'
'    End Select
'
'    EOBCurIndex = EOBCurIndex + 1
'End Sub
'
'sub EndOfBallStart
'pDMDSetPage 4  'set to EOB screen
'EOBCurIndex=0
'EndOfBallNext
'end Sub
'
'
'Sub pDMDSplashLinesEOB(msgText,msgText2,timeSec,mColor)
'	PuPlayer.LabelSet pDMD,"EOBLine1",msgText,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"
'	PuPlayer.LabelSet pDMD,"EOBLine2",msgText2,0,"{'mt':1,'at':1,'fq':250,'len':" & (timeSec*1000) & ",'fc':" & mColor & "}"   
'end Sub
'---------
Sub pDMDmessage1L(msgText,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte1a",msgText,vis,pLine1Ani
	DmDmessageOFFTimer.Enabled = True
end Sub
Sub pDMDmessage2L(msgText,msgText2,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte2a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Texte2b",msgText2,vis,pLine2Ani
	DmDmessageOFFTimer.Enabled = True
end Sub

Sub pDMDmessage3L(msgText,msgText2,msgText3,timeSec)
	DmDmessageOFFTimer.Enabled = False
	Dim vis:vis=1
	if pLine1Ani<>"" Then vis=0
	PuPlayer.LabelSet pDMD,"Texte3a",msgText,vis,pLine1Ani
	PuPlayer.LabelSet pDMD,"Texte3b",msgText2,vis,pLine2Ani
	PuPlayer.LabelSet pDMD,"Texte3c",msgText3,vis,pLine3Ani
	DmDmessageOFFTimer.Enabled = True
end Sub

	'need to make a timer and add and remove to it instead of showtext and hide text
Sub DmDmessageOFFTimer_Timer()
	ResetMessage123L
End Sub 
Sub ResetMessage123L
	DmDmessageOFFTimer.Enabled = False
	PuPlayer.LabelSet pDMD, "Texte1a","",0,""
	PuPlayer.LabelSet pDMD, "Texte2a","",0,""
	PuPlayer.LabelSet pDMD, "Texte2b","",0,""
	PuPlayer.LabelSet pDMD, "Texte3a","",0,""
	PuPlayer.LabelSet pDMD, "Texte3b","",0,""
	PuPlayer.LabelSet pDMD, "Texte3c","",0,""
End Sub 
'****************************************************************************************************************************************
' ATTRACT MODE
'****************************************************************************************************************************************
Dim AttractIndex

Sub pAttractStart
    pInAttract = True 
    hsbModeActive = False
    pDMDSetPage 3
    AttractIndex = 0
    AttractNext
End Sub


Sub AttractNext
    If pInAttract = False Then Exit Sub

    Select Case AttractIndex
			' Use fade/pulse effect instead of blinking
		Case 0
			pDMDSplashLinesAttract2 "", "", "","","", 2
			pDMDSplashLinesAttract "", "", "","","", 2
			TriggerScript 500, "AttractNext" 

        Case 1 
            pDMDSplashLinesAttract "", "","","","Game Over", 2
            TriggerScript 7000, "AttractNext" 
			If usePuPDMD=False Then	PlaySong "Mu_Intro"
			PuPEvent 858
			pDMDLabelHide "Overlay"
			Playerscore(CurrentPlayer) = 0
			AnimTrailerTimer.Enabled = True
            AttractIndex = 1 
			
        Case 2
			pDMDSplashLinesAttract "", "", "","","", 2
           pDMDSplashLinesAttract2 _
           "High Scores", _
			"1. " & HighScoreName(0) & " - " & FormatNumber(HighScore(0), 0), _
            "2. " & HighScoreName(1) & " - " & FormatNumber(HighScore(1), 0), _
            "3. " & HighScoreName(2) & " - " & FormatNumber(HighScore(2), 0), _
            "Aladdin", _
            4000
            TriggerScript 7000, "AttractNext"

		Case 3
			pDMDSplashLinesAttract2 "", "", "","","", 2
			If(Credits> 0) then
				pDMDSplashLinesAttract "Press", "Start","","","Game Over", 2
			Else
				pDMDSplashLinesAttract "Insert", "Coins","","","Game Over", 2
			End If
            TriggerScript 7000, "AttractNext"

        Case 4
            pDMDSplashLinesAttract "By", "Tombg", "","","Aladdin", 2
            TriggerScript 14000, "AttractNext"

		Case 5
			pDMDSplashLinesAttract "if Jasmine", "is in love with you","","","Replay", 7
            TriggerScript 7000, "AttractNext"

		Case 6
			If(Credits> 0) then
				pDMDSplashLinesAttract "Press", "Start","","","Game Over", 5
			Else
				pDMDSplashLinesAttract "Insert", "Coins","","","Game Over", 5
			End If
            TriggerScript 5000, "AttractNext"
            AttractIndex = -1 
    End Select

    AttractIndex = AttractIndex + 1
End Sub


Sub pDMDSplashLinesAttract(msgTitle, msgText1, msgText2, msgText3, msgText4, timeMs)
    PuPlayer.LabelSet pDMD, "AttractTitle", msgTitle, 1, "" 
    PuPlayer.LabelSet pDMD, "AttractLine1", msgText1, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine2", msgText2, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine3", msgText3, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine4", msgText4, 1, ""
End Sub
Sub pDMDSplashLinesAttract2(msgTitle, msgText1, msgText2, msgText3, msgText4, timeMs)
    PuPlayer.LabelSet pDMD, "AttractTitle", msgTitle, 1, "" 
    PuPlayer.LabelSet pDMD, "AttractLine1Alt", msgText1, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine2Alt", msgText2, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine3Alt", msgText3, 1, ""
    PuPlayer.LabelSet pDMD, "AttractLine4", msgText4, 1, ""
End Sub

Sub AttractEnd
    AttractIndex = 0                ' reset index
    pInAttract = False              ' stops AttractNext from doing anything
	PuPEvent 859
	pDMDLabelshow "Overlay"
	AnimTrailerTimer.Enabled = False
	pDMDSplashLinesAttract "", "","","","", 2
    TriggerScript 2000, "ResetHUD"
End Sub

Sub ResetHUD()
    PuPEvent 859
	pDMDLabelshow "Overlay"
	AnimTrailerTimer.Enabled = False
	pDMDSetPage pScores
End Sub

Sub pDMDGameOver
	playclear pDMD
	pAttractStart
end Sub

Sub pDMDStartGame
	AttractEnd
end Sub

'----------------
'************** Nailbuster TriggerScript Code v1.20
' create a timer in table named exactly pTriggerScript
' set interval on timer to 100ms (not enabled on startup)
' 
'  currently support up to 10 concurrent timers
'
'
'   usage:    TriggerScript <ms>, "vbcode to execute"
'
'   simpletest:  TriggerScript 3500,"MsgBox 1234"   'will show a dialog 1234
'

'Const TriggerScriptSize=10
'Dim pReset(10)                 ' TriggerScriptSize
'Dim pStatement(10)             ' TriggerScriptSize - holds future scripts
'Dim FX

Sub TriggersStopAll()
for fx=0 to TriggerScriptSize
    pReset(FX)=0
    pStatement(FX)=""
next
pTriggerScript.Enabled=False  'YOU MUST HAVE A TIMER NAMED pTriggerScript interval 100 not active on startup.
end Sub

TriggersStopAll



DIM pTriggerCounter:pTriggerCounter=pTriggerScript.interval    'YOU MUST HAVE A TIMER NAMED pTriggerScript interval 100 not active on startup.

Sub pTriggerScript_Timer()
    dim bMoreToRun:bMoreToRun=False
    for fx=0 to TriggerScriptSize  
        if pReset(fx)>0 Then    
            pReset(fx)=pReset(fx)-pTriggerCounter 
            if pReset(fx)<=0 Then
                pReset(fx)=0
                execute(pStatement(fx))
            end if
            bMoreToRun=True            
        End if
    next
    if bMoreToRun = False then pTriggerScript.Enabled=False    ' Disable when we dont need it 
End Sub

Sub TriggerScript(pTimeMS, pScript) ' This is used to Trigger script after the pTriggerScript Timer
    for fx=0 to TriggerScriptSize  
        if pReset(fx)=0 Then
            pReset(fx)=pTimeMS
            pStatement(fx)=pScript
            if pTriggerScript.enabled=false Then pTriggerScript.enabled=true
            Exit Sub
        End If 
    next
end Sub
'--------------------------------
'--- BONUS SEQUENCE
'--------------------------------
Sub resetbackglassOFFScore
	PuPlayer.LabelSet pDMD,"BonusMultip", FormatNumber(BonusMultiplier,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus1", FormatNumber(SlingCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus2", FormatNumber(RampCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus3", FormatNumber(BumperCount,0),0,""
	PuPlayer.LabelSet pDMD,"Bonus4", FormatNumber(MissionCount,0),0,""
	PuPlayer.LabelSet pDMD,"BonusTotal", FormatNumber(TotalBonus,0),0,""
End Sub

Function BonusScoreTotal()
	BonusScoreTotal = BonusMultiplier * ((SlingCount * 500) + (RampCount * 1000) + (BumperCount * 5000) + (MissionCount * 8000) )
End Function 
Sub SeqBonus
'	PauseDMDHarem = 1
	ResetMessage123L
	GiOff
	vpmtimer.addtimer 200, "SeqBonus1 '"
	vpmtimer.addtimer 650, "SeqBonus2 '"
	vpmtimer.addtimer 1120, "SeqBonus3 '"
	vpmtimer.addtimer 1570, "SeqBonus4 '"
	vpmtimer.addtimer 2120, "SeqBonus5 '"
End Sub
Sub SeqBonus1
	PuPlayer.LabelSet pDMD,"BonusMultip","Bonus x " & FormatNumber(BonusMultiplier,0),1,""
	PuPlayer.LabelSet pDMD,"Bonus1", "Experience = " & FormatNumber((SlingCount * 500),0),1,""
'	PlaySound "Bat"
End Sub
Sub SeqBonus2
	PuPlayer.LabelSet pDMD,"Bonus2", "Ramps = " & FormatNumber((RampCount * 1000),0),1,""
End Sub
Sub SeqBonus3
	PuPlayer.LabelSet pDMD,"Bonus3", "Guard Hits = " & FormatNumber((BumperCount * 5000),0),1,""
End Sub
Sub SeqBonus4
	PuPlayer.LabelSet pDMD,"Bonus4", "Harem Girls = " & FormatNumber((MissionCount * 8000),0),1,""
End Sub
Sub SeqBonus5
'	PuPlayer.LabelSet pDMD,"BonusTotal", "Total Bonus = " & FormatNumber(BonusScoreTotal,0),1,""
'	PlaySound "Bat2"
	pDMDLabelPulseNumber "BonusTotal", "Total Bonus = " & FormatNumber(BonusScoreTotal,0), 1000, clWhite, 0, BonusScoreTotal, 1
	PlayerScore(CurrentPlayer) = PlayerScore(CurrentPlayer) + BonusScoreTotal
End Sub