{\rtf1\mac\ansicpg10000\cocoartf102
{\fonttbl\f0\fnil\fcharset77 Monaco;\f1\fswiss\fcharset77 Helvetica;}
{\colortbl;\red255\green255\blue255;\red0\green0\blue0;\red0\green0\blue191;\red96\green96\blue96;
\red191\green0\blue0;\red0\green115\blue0;}
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\ql\qnatural

\f0\fs18 \cf2 (\
s = \cf3 Server\cf2 .internal;\
\
\cf3 SynthDef\cf2 (\cf4 "sc303"\cf2 , \{ \cf3 arg\cf2  out=0, freq=440, wave=0, ctf=100, res=0.2, \
		sus=0, dec=1.0, env=1000, gate=0, vol=0.2;\
	\cf3 var\cf2  filEnv, volEnv, waves;\
	\
	\cf5 // can't use adsr with exp curve???\cf2 \
	\cf5 //volEnv = EnvGen.ar(Env.adsr(1, 0, 1, dec, vol, 'exp'), In.kr(bus));\cf2 \
	volEnv = \cf3 EnvGen\cf2 .ar(\cf3 Env\cf2 .new([10e-10, 1, 1, 10e-10], [0.01, sus, dec], \cf6 'exp'\cf2 ), gate);\
	filEnv = \cf3 EnvGen\cf2 .ar(\cf3 Env\cf2 .new([10e-10, 1, 10e-10], [0.01, dec], \cf6 'exp'\cf2 ), gate);\
	\
	waves = [\cf3 Saw\cf2 .ar(freq, volEnv), \cf3 Pulse\cf2 .ar(freq, 0.5, volEnv)];\
	\
	\cf3 Out\cf2 .ar(out, \cf3 RLPF\cf2 .ar( \cf3 Select\cf2 .ar(wave, waves), ctf + (filEnv * env), res).dup * vol);\
\}).send(s);\
\
)\
\
\cf5 /*\
s.sendMsg("s_new", "sc303", 3000, 0, 0);\
s.sendMsg("n_set", 3000, 'gate', 1, 'freq', 220);\
s.sendMsg("n_set", 3000, 'gate', 0);\
s.sendMsg("n_set", 3000, 'dec', 1);\
\
s.sendMsg("n_free", ~node) \
*/\cf2 \
\
\
\cf5 // make GUI!\cf2 \
(\
~bpm = 140;\
~step = 0;\
	\cf5 // set default note ons\cf2 \
~defaultNoteOns = \cf3 Array\cf2 .fill(16,1);\
	\cf5 // this is the note on array that will be used in the sequencer routine\cf2 \
~noteOns = ~defaultNoteOns;\
\
	\cf5 // set root\cf2 \
~root = 36;\
\
	\cf5 // set default pitches\cf2 \
~defaultPitches = [ 0, 12, 0, 0, -12, 0, 0, 0, -12, 0, 12, 0, 3, -5, 2, 0 ];\
	\cf5 // this is the pitch array that will be used in the sequencer routine\cf2 \
~pitches = ~defaultPitches;\
\
~tStepLED = \cf3 Array\cf2 .new; 	\cf5 // the step indicator LEDs as an array\cf2 \
~sNoteOn = \cf3 Array\cf2 .new;  	\cf5 // the note-on sliders as an array\cf2 \
~tNoteOn = \cf3 Array\cf2 .new;	\cf5 // the note-on value boxes as an array\cf2 \
~sPitch = \cf3 Array\cf2 .new;	\cf5 // the pitch sliders as an array\cf2 \
~tPitch = \cf3 Array\cf2 .new; 	\cf5 // the pitch value boxes as an array\cf2 \
\
w = \cf3 SCWindow\cf2 (\cf4 "SC-303"\cf2 , \cf3 Rect\cf2 (128, 64, 510, 320));\
w.view.background = \cf3 Color\cf2 .white;\
w.front;\
\
~layoutLeft = \cf3 SCCompositeView\cf2 (w, \cf3 Rect\cf2 (0, 0, 400, 320));	\cf5 // make the left pane of the window\cf2 \
~layoutLeft.decorator = \cf3 FlowLayout\cf2 (~layoutLeft.bounds); \cf5 // auto-arrange the elements\cf2 \
~layoutRight = \cf3 SCCompositeView\cf2 (w, \cf3 Rect\cf2 (400, 0, 400, 120));\
~layoutRight.decorator = \cf3 FlowLayout\cf2 (~layoutRight.bounds);\
\
\cf5 // make transport controls\cf2 \
\
	\cf5 // Tempo\cf2 \
\cf3 SCStaticText\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 45, 14)).string_(\cf4 "BPM: "\cf2 );\
~tBpm = \cf3 SCNumberBox\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 45, 14))\
	.string_(~bpm)\
	.action_(\{ \cf3 arg\cf2  item;\
		~bpm = item.value;\
		~sBpm.value_((~bpm-40)/200);\
	\});\
\
~layoutRight.decorator.nextLine;\
~sBpm = \cf3 SCSlider\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 100, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.value_((~bpm-40)/200)\
	.action_(\{ \cf3 arg\cf2  sldr;\
		~bpm = ((sldr.value)*200) + 40;\
		~tBpm.string_(~bpm);\
	\});\
\
~layoutRight.decorator.nextLine;\
\
	\cf5 // Reset\cf2 \
\cf3 SCButton\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0,45,20))\
	.states_([[\cf4 "|<"\cf2 ]])\
	.action_(\{ \cf3 arg\cf2  item;\
		~tStepLED[~step].background_(\cf3 Color\cf2 .new(0.6,0,0));\
		~step = 0;\
		~tStepLED[0].background_(\cf3 Color\cf2 .new(1,0,0));\
	\});\
\
	\cf5 // Play/Pause\cf2 \
\cf3 SCButton\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0,45,20))\
	.states_([[\cf4 ">"\cf2 ],[\cf4 "||"\cf2 ]])\
	.action_(\{ \cf3 arg\cf2  item;\
		if( item.value == 0, \{\
			~seqr.stop;\
		\},\{\
			~seqr.reset;\
			~seqr.play;\
		\});\
	\});\
\
~layoutRight.decorator.nextLine;\
\
	\cf5 // Root note\cf2 \
\cf3 SCStaticText\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 45, 14)).string_(\cf4 "Root: "\cf2 );\
~tRoot = \cf3 SCStaticText\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 45, 14)).string_(~root);\
~layoutRight.decorator.nextLine;\
\cf3 SCSlider\cf2 (~layoutRight, \cf3 Rect\cf2 (0,0, 100, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.value_((~root-24)/36)\
	.step_(1/48)\
	.action_(\{ \cf3 arg\cf2  sldr;\
		~root = ((sldr.value)*36) + 24;\
		~tRoot.string_(~root);\
	\});\
\
	\
\cf5 // make synth control labels\cf2 \
\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 45, 14)).string_(\cf4 "Wave"\cf2 );\
\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 14)).string_(\cf4 "Cutoff"\cf2 );\
\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 14)).string_(\cf4 "Resonance"\cf2 );\
\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 14)).string_(\cf4 "Decay"\cf2 );\
\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 14)).string_(\cf4 "Envelope"\cf2 );\
~layoutLeft.decorator.nextLine;\
\
\cf5 // make the synth controls\cf2 \
	\cf5 // wave-type\cf2 \
\cf3 SCButton\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,45,20))\
	.states_([[\cf4 "Saw"\cf2 ],[\cf4 "Square"\cf2 ]])\
	.action_(\{ \cf3 arg\cf2  item;\
		s.sendMsg(\cf4 "n_set"\cf2 , ~node, \cf6 'wave'\cf2 , item.value);\
	\});\
	\cf5 // cut-off frequency\cf2 \
\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.action_(\{ \cf3 arg\cf2  sldr;\
		s.sendMsg(\cf4 "n_set"\cf2 , ~node, \cf6 'ctf'\cf2 , ((sldr.value)*(10000-100))+100); \cf5 // modify synth param\cf2 \
	\});\
	\
	\cf5 // resonance amt\cf2 \
\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.action_(\{ \cf3 arg\cf2  sldr;\
		s.sendMsg(\cf4 "n_set"\cf2 , ~node, \cf6 'res'\cf2 , (1-sldr.value)*(0.97)+0.03); \cf5 // modify synth param\cf2 \
	\});\
	\
	\cf5 // decay amt\cf2 \
\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.action_(\{ \cf3 arg\cf2  sldr;\
		s.sendMsg(\cf4 "n_set"\cf2 , ~node, \cf6 'dec'\cf2 , (sldr.value)*2); \cf5 // modify synth param\cf2 \
	\});\
	\
	\cf5 // envelope amt\cf2 \
\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0, 80, 20))\
	.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
	.action_(\{ \cf3 arg\cf2  sldr;\
		s.sendMsg(\cf4 "n_set"\cf2 , ~node, \cf6 'env'\cf2 , (sldr.value)*10000); \cf5 // modify synth param\cf2 \
	\});\
\
~layoutLeft.decorator.nextLine;\
\
	\cf5 // make step LEDs\cf2 \
16.do(\{ \cf3 arg\cf2  i;\
	~tStepLED = ~tStepLED.add(\
		\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,20,12))\
			.background_(\cf3 Color\cf2 .new(0.6,0,0))\
			.stringColor_(\cf3 Color\cf2 .white)\
			.string_(i+1)\
	);\
\});\
~tStepLED[~step].background_(\cf3 Color\cf2 .new(1,0,0));\
~layoutLeft.decorator.nextLine;\
\
	\cf5 // make the note-on sliders\cf2 \
16.do(\{ \cf3 arg\cf2  i;\
	~sNoteOn = ~sNoteOn.add(\
		\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,20,60))			\cf5 // position doesn't matter; width, height do\cf2 \
			.background_(\cf3 Color\cf2 .new(0,0,0.7))		\cf5 // color the slider\cf2 \
			.step_(0.1)						\cf5 // values are stepped by 0.1\cf2 \
			.value_(~noteOns[i])				\cf5 // assign it its value from the note on array\cf2 \
			.action_(\{ \cf3 arg\cf2  sldr;				\cf5 // when the slider is moved...\cf2 \
				~noteOns[i] = sldr.value;		\cf5 // ... update note-on array\cf2 \
				~tNoteOn[i].string = sldr.value; \cf5 // ... update its value box\cf2 \
					\cf5 // ...change color of value box\cf2 \
				~tNoteOn[i].background = \cf3 Color\cf2 .new((1-~noteOns[i]),1,(1-~noteOns[i])); \
			\})\
	);\
\});\
~layoutLeft.decorator.nextLine;\
\
	\cf5 // make the note-on value boxes\cf2 \
16.do(\{ \cf3 arg\cf2  i;\
	~tNoteOn = ~tNoteOn.add(\
		\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,20,20))\
			.background_(\cf3 Color\cf2 .new((1-~noteOns[i]),1,(1-~noteOns[i])))\
			.string_(~sNoteOn[i].value)\
			.align_(0)\
			.action_(\{ \cf3 arg\cf2  text;\
				text.string.postln;\
			\})\
	);\
\});\
~layoutLeft.decorator.nextLine;\
\
	\cf5 // make the pitch sliders\cf2 \
16.do(\{ \cf3 arg\cf2  i;\
	~sPitch = ~sPitch.add(\
		\cf3 SCSlider\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,20,120))\
			.value_((~pitches[i]+12)/24)\
			.step_(1/24)\
			.background_(\cf3 Color\cf2 .new(0.8,0.8,0.8))\
			.action_(\{ \cf3 arg\cf2  item;\
				~pitches[i] = ((item.value * 24)-12);\
				~tPitch[i].string = ((item.value * 24)-12);\
			\})\
	);\
\});\
~layoutLeft.decorator.nextLine;\
\
	\cf5 // make the pitch value boxes\cf2 \
16.do(\{ \cf3 arg\cf2  i;\
	~tPitch = ~tPitch.add(\
		\cf3 SCStaticText\cf2 (~layoutLeft, \cf3 Rect\cf2 (0,0,20,20))\
			.background_(\cf3 Color\cf2 .white)\
			.string_(~pitches[i].value)\
			.align_(0)\
	);\
\});\
\cf5 /*~tempButt = SCButton(w, Rect(0,0,60,60))\
			.states_([\
				["0", Color.red, Color.black]\
			]);*/\cf2 \
\
)\
\
\cf5 // do other stuff\cf2 \
\
(\
~node = s.nextNodeID;\
\
\cf5 // start the synth\cf2 \
s.sendMsg(\cf4 "s_new"\cf2 , \cf4 "sc303"\cf2 , ~node, 1, 1);\
\
\cf5 // make the step sequencer\cf2 \
~seqr = \cf3 Routine\cf2 .new(\{\
	loop(\{\
			\
			\cf5 // turn on LED\cf2 \
		\{~tStepLED[~step].background_(\cf3 Color\cf2 .new(1,0,0)); \cf5 /*(~step.asString + "on").postln;*/\cf2 \}.defer;\
	\
		if(~noteOns[~step].coin, \{\
			\cf5 // play note!\cf2 \
				\cf5 // this is out of sync\cf2 \
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\ql\qnatural

\f1\fs24 \cf5 //
\f0\fs18 \cf5 			s.sendMsg("n_set", ~node, 'gate', 1, 'freq', (~pitches[~step]+~root).midicps);\cf2 \

\f1\fs24 \cf5 //
\f0\fs18 \cf5 			(7.5/~bpm).wait;\cf2 \

\f1\fs24 \cf5 //
\f0\fs18 \cf5 			s.sendMsg("n_set", ~node, 'gate', 0);\cf2 \

\f1\fs24 \cf5 //
\f0\fs18 \cf5 			(7.5/~bpm).wait;\cf2 \
			\
				\cf5 // buffer playback on server (adds start/stop delay, but better inter-note timing)\cf2 \
			s.sendBundle(s.latency, [\cf4 "n_set"\cf2 , ~node, \cf6 'gate'\cf2 , 1, \cf6 'freq'\cf2 , (~pitches[~step]+~root).midicps]);\
			(7.5/~bpm).wait;\
			s.sendBundle(s.latency, [\cf4 "n_set"\cf2 , ~node, \cf6 'gate'\cf2 , 0]);\
			(7.5/~bpm).wait;\
		\},\{\
				\cf5 // it's a rest\cf2 \
			(15/~bpm).wait;\
		\});\
		\
			\cf5 // turn off LED\cf2 \
		\{~tStepLED[(~step-1)%16].background_(\cf3 Color\cf2 .new(0.6,0,0)); \cf5 /*(((~step-1)%16).asString + "off").postln;*/\cf2 \}.defer;\
		\
		~step = (~step + 1)%16;	\
		\
	\});\
\});\
)\
\
~seqr.play;\
~seqr.stop;\
~seqr.reset;\
~pitches\
}