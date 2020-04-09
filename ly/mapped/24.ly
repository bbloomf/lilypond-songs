\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2.4)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . -5)
       (minimum-distance . -5)
       (padding . -14)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #24
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key ees \major
  \time 3/4
  \tempo 4 = 126
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusicFirst = \relative c' {
	bes'8[ c] bes[ g] bes[ ees] |
  d4-. d2-- |
  c4-. c2-- |
  bes4-. bes2-- |
  c8[ bes] g[ c] bes[ g] |
  aes4 aes2-- |
  
  c4-. c2-- |
  bes4-. bes2-- |
  bes8[ c] bes[ g] bes[ ees] |
  d4-. d2-- |
  c4-. c2-- |
  bes4-. bes2 -- |
  c8[ bes] g[ c] bes[ g] |
  
  %page2
  aes4 c,2-- |
  g'4-. f2-- |
  ees2. |
  bes'2.\rest |
  bes4\rest bes4\rest aes4-. |
  g4( d) g-. |
  g2. |
  
  aes8[ bes] c4. ees8 |
  ees4-. d2-- |
  aes2 aes4 |
  aes4( g2) |
  b2.\rest |
  b4\rest b\rest aes |
  g( d) g |
  
  %page3
  g2. |
  aes4-. aes2-> |
  g4-. g2-> |
  ees8[ f]( ees4) d |
  c2. |
  bes'8[ c] bes[ g] bes[ ees] |
  d4-. d2-- |
  
  c4-. c2-- |
  bes4-. bes2-- |
  c8[ bes] g[ c] bes[ g] |
  aes4-. aes2-- |
  c4-. c2-- |
  bes4-. bes2-- |
  bes8[ c] bes[ g] bes[ ees] |
  
  %page4
  d4-. d2-- |
  c4-. c2-- |
  bes4-. bes2-- |
  c8[ bes] g[ c] bes[ g] |
  aes4-. c,2-- |
  g'4-. f2-- |
  ees2. |
  
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  %\once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \once \override Score.RehearsalMark #'extra-offset = #'( 0 . -1 )
  \mark \markup\musicglyph #"scripts.coda"
  \break
}
sopMusicSecond = \relative c' {
  \key aes\major
  \repeat unfold 6 r2. |
  
  %page5
  \repeat unfold 6 r2. |
  
  \repeat unfold 4 r2. | \pageBreak
  c'8.\f c16 f4-> c\< |
  des8.[ c16]\! bes2-> |
  
  %page6 (sop)
  c8.[ des16] c8 bes aes[ g] |
  aes16[ bes aes g] f2 |
  c'4-.\cresc f2->\! |
  des8.[ c16] bes2-> |
  c8.[^\markup\italic"rall." des16] c8[ bes] aes[ g] |
  f2. |
  
  <bes aes>8.-.\p <c aes>16 <des aes>4-. <des aes>-. |
  r2. %bes8.-. c16 des4-. des-. |  %(Tenors sing this line NOT sopranos)
  c8.[\ff des16]^\markup\italic"molto rall." c8 bes aes[ g] |
  f2\> ees4\!\fermata |
  r2. |
  
  %page7
  \repeat unfold 3 r2.
  aes4-.\mp aes2-> |
  
  aes4-. aes2-> |
  ees4 ees d |
  d( ees2) |
  c'4.\mf des8 d ees |
  f[ ees] aes,4-. aes-. |
  
  %page8
  aes8\< g des'4-.\! des-. |
  f,8 ees c'4-. c-. |
  c4. des8 d ees |
  f ees aes,4-. aes-. |
  
  g8[ des'] f,4-. g-. |
  aes2. \bar"||"
  r2. |
  bes2.-> |
  
  %page9
  bes2.-> |
  bes2.->\fermata \bar"||"
  \mark"D.C. al Coda"
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  c2->\ff^\tweak #'X-offset #-3
  ^\markup\musicglyph #"scripts.coda" c4-> |
  ees2.-> |
  c,4-> c-> c-> |
  ees2.->\fermata \bar"|."
}
sopWords = \lyricmode {
	Come ye where gold of May is shin -- ing,
  Come ye where buds of flow’rs are twin -- ing;
  As to the bells of fair -- ies chim -- ing,
  Trip we thro’ bow’rs of ra -- diant \set associatedVoice = "tenorsFirst" Spring.
  
  
  Glad -- some the morn -- ing,
  %\unset associatedVoice
  
  \set associatedVoice = "altosFirst"
  The land is gay,
  O -- ver the mead -- ows trip \unset associatedVoice (trip) a -- \set associatedVoice = "basses" way;
  
  
  Ech -- oes the brook -- let
  
  \set associatedVoice = "altosFirst"
  by wood and lea: “Sing, sing, O heart, be glad with me!”
  
  \unset associatedVoice
  Come ye where gold of May is shin -- ing,
  Come ye where buds of flow’rs are twin -- ing,
  As to the bells of fair -- ies chim -- ing,
  Trip we thro’ bow’rs of ra -- diant Spring.
  
  
  Light of Day re -- turn -- eth, glo -- ry of Spring burn -- eth;
  Joy notes peal -- ing, gay mu -- sic make.
  Light re -- turn -- eth,
  %Light re -- turn -- eth,
  Glo -- ry of Spring burn -- eth;
  
  “Wel -- come, Wel -- come, Wel -- come the May!”
  Gai -- ly is the lark sing -- ing,
  Up -- ward wing -- ing, glad -- ness ring -- ing,
  Un -- to all the mes -- sage bring -- ing:
  “Wel -- come the May!”
  
  Come, oh, come.
  “Sing, O heart! be glad with me!”
}

sopWordsII = \lyricmode {
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusicFirst = \relative c' {
  g'4 ees g |
  aes4-. aes2-- |
  aes4-. aes2-- |
  g4-. g2-. |
  ees4 ees ees |
  f4-. f2-- |
  
  aes4-. aes2-- |
  g4-. g2-- |
  g4 ees g |
  aes4-. aes2-- |
  aes4-. aes2-- |
  g4-. g2-- |
  g4 g g |
  
  %page2 (alto)
  f4-. c2-- |
  c4-. d2-- |
  ees2.-- |  
  s2. |
  s4 s ees4-. |
  d2-- d4-. |
  ees8[ d]( c2) |
  
  f8[ g] aes4. c8 |
  c4-. bes2-- |
  c,8[-> d] ees4.-> d8 |
  c4->( bes2) |
  s2. |
  s4 s ees4-. |
  d2-- d4-. |
  
  %page3 (alto)
  ees8[ d]( c4) g'4-. |
  f4-. f2-> |
  c4-. c2-> |
  b4-. b2 |
  c2. |
  g'4 ees g |
  aes4-. aes2-- |
  
  aes4-. aes2-- |
  g4-. g2-- |
  ees4 ees ees |
  f4-. f2-- |
  aes4-. aes2-- |
  g4-. g2-- |
  g4 ees g |
  
  %page4 (alto)
  aes4-. aes2-- |
  aes4-. aes2-- |
  g4-. g2-- |
  g4 g g |
  f4-. c2-- |
  c4-. d2-- |
  ees2. |
}
altoMusic = \relative c' {
  \repeat unfold 48 s2. |
  
  \key aes\major
  \tempo 4 = 104
  c4.\(\mp des8 d ees |
  f[ ees] aes4\) aes4-. |
  aes8\( g des'4-.\) f, |
  f8[\( ees] c'4-.\) c, |
  c4.\(\cresc des8\! d[ ees] |
  f[ ees] aes4\) aes |
  
  %page5 (alto)
  g8\( bes bes,4\) f'-. |
  f-.\> ees->( des)\! |
  c4.\(\mp des8 d ees |
  f[ ees] aes4\) aes4-. |
  aes8[\( g] des'4-.\) f,-. |
  f8[\( ees] c'4-.\) c,-. |
  
  c4.\(\cresc des8\! d ees |
  f[ ees] aes4-.\) aes-. |
  g8[ des']( f,4-.) g-. |
  aes2.-- |
  aes8. aes16 f4-> f |
  f4-. f2-> |
  
  %page6 (alto)
  c'8.[ des16] c8 bes aes[ g] |
  aes16[ bes aes g] f2 |
  aes4-. c2-> |
  f,4-. f2-> |
  f4 e c |
  c2. |
  
  <f des>8.-. <f c>16 <f bes,>4-. q4-. |
  r2. |
  c'8.[ des16] c8 bes aes[ g] |
  f2 des4 |
  c4.\(^\mp des8^\markup\italic"a tempo" d ees |
  
  %page7 (alto)
  f8[ ees] aes4-.\) aes-. |
  aes8\( g des'4-.\) des-. |
  f,8\( ees c'4-.\) c-. |
  c,4.\( des8 d ees |
  
  f ees aes4-.\) aes-. |
  g8->[ bes] \parenthesize bes,4-> f'-> | %parenthetic note MUST be checked
  f4->( ees des) |
  c4.\( des8 d ees |
  f[ ees] aes4-.\) aes-. |
  
  %page8 (alto)
  aes8\( g des'4-.\) des-. |
  f,8\( ees c'4-.\) c-. |
  c,4.\( des8 d ees |
  f ees aes4-.\) aes-. |
  
  g8[ des'] f,4-. g |
  aes2. |
  r2. |
  aes2.-> |
  
  %page9
  bes,8-> bes-> c4->\< d-> |
  <d g>2->\!( <d f>4\fermata) \bar"||"
  ees8[ f] ees[ c] ees[ aes] |
  g2.-> |
  <aes, \tweak #'font-size #-3 aes'>4-> aes-> aes-> |
  bes2.-> \bar"|."
}
altoWords = \lyricmode {
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing;
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
%  The land is gay,
%  O -- ver the mead -- ows trip, trip a -- way;
%  By wood and lea:
%  “Sing, sing, O heart, be glad with me!”
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing,
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
  
  
  Trip we, oh, so light -- ly, where dew -- y grass is sway -- ing,
  Where ’mid the fair blos -- som the but -- ter flies are stray -- ing.
  ’Tis the hour of play -- ing; all voi -- ces are say -- ing:
  “Come, come ye forth a May -- ing; to joy a -- wake!”
  Light of Day re -- turn -- eth, glo -- ry of Spring burn -- eth;
  Joy -- notes peal -- ing, gay mu -- sic make.
  Light re -- turn -- eth,
  Glo -- ry of Spring burn -- eth;
  
  Gai -- ly is the lark sing -- ing, Up -- ward wing -- ing, glad -- ness ring -- ing,
  Un -- to all the mes -- sage bring -- ing: “Wel -- come the May!”
  Gai -- ly is the lark sing -- ing,
  Up -- ward wing -- ing, glad -- nes ring -- ing,
  Un -- to all the mes -- sage bring -- ing: “Wel -- come the May!”
  Come, light -- ly trip -- ping, come.
  “Sing, sing, O heart! be glad with me!”
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusicFirst = \relative c' {
  %page1 (82) tenors
  ees4 ees ees |
  bes4-. bes2-- |
  d4-. d2-- |
  ees4-. ees2-- |
  ees4 ees ees |
  c4-. c2-- |
  
  d4-. d2-- |
  ees4-. ees2-- |
  ees4 ees ees |
  bes4-. bes2-- |
  d4-. d2-- |
  ees4-. ees2-- |
  e4 c e |
  
  %page2 (83) tenors
  c4 aes2 |
  bes4-. aes2-- |
  g2. |  
  g4-> c-> g-> |
  aes8[ g] f4-. c'-. |
  b2-- g4-. |
  g8[ f]( ees2) |
  
  f2-> f4-. |
  bes-. bes2-> |
  r2. r2. |
  g4->\f c-> g-> |
  aes8[ g] f4-. c'-. |
  b2-- g4-. |
  
  %page3 (84) tenors
  g8[ f]( ees4) c'-. |
  c8[ aes]( f4) c' |
  c8[ g]( ees4) c'4 |
  g2 f4 |
  ees'2. |
  ees4 ees ees |
  bes4-. bes2-- |
  
  d4-. d2-- |
  ees4-. ees2-- |
  ees4 ees ees |
  c4-. c2-- |
  d4-. d2-- |
  ees4-. ees2-- |
  ees4 ees ees |
  
  %page 4 (85) tenors
  bes4-. bes2-- |
  d4-. d2-- |
  ees4-. ees2-- |
  e4 c e |
  c4 aes2-- |
  bes4-. aes2-- |
  g2.
}
tenorMusic = \relative c' {
  \repeat unfold 48 s2. |
  
  \key aes\major
  aes4.\( bes8 b c |
  des4 c\) c-. |
  bes8\( des f4-.\) des-. |
  aes8[\( c] ees4-.\) aes,-. |
  aes4.\( bes8 b[ c] |
  des4 c-.\) c-. |
  
  %page 5 (86) tenors
  bes8\( bes g4-.\) aes4-. |
  aes4-. g2-- |
  aes4.\( bes8 b c |
  des4 c\) c4-. |
  bes8[\( des] f4-.\) des-. |
  aes8[ c] ees4-. aes,-. |
  
  aes4.\( bes8 b c |
  des4 c-.\) c-. |
  des4-. des2-> |
  c2.-- |
  c8. c16 c4-> c |
  bes4-. bes2-> |
  
  %page 6 (87) tenors
  c8.[ des16] c8 bes aes[ g] |
  aes16[ bes aes g] f2 |
  f4-. f2-> |
  bes4-. bes2-> |
  aes8.[ bes16] c8[ des] c[ bes] |
  aes2. |
  
  r2. |
  <bes aes>8. <c aes>16 <des aes>4-. q-. |
  c8.[ des16] c8 bes aes[ g] |
  f2 bes4 |
  ees4-. ees2->( |
  
  %page 7 (88) tenors
  des8[) ees]( c2) |
  des4--( f2->)( |
  c4--)( ees2->) |
  ees4-. ees2->\( |
  
  des8[ ees] c2\) |
  bes4( g)( f)( |
  aes)( g2) |
  aes4.\( bes8 b c |
  des4 c-.\) c-. |
  
  %page 8 (89) tenors
  bes8\( des f4-.\) f-. |
  aes,8\( c ees4-.\) ees4-. |
  aes,4.\( bes8 b c |
  des des c4-.\) c-. |
  
  des4-. des2-> |
  c2. |
  r2. |
  ees8-> ees-> d4-> d-> |
  
  %page 9 (90) tenors
  d,4->( ees) f-> |
  aes2.->\fermata \bar"||"
  aes2-> aes4-> |
  bes2->( ees,4) |
  ees8[ f] ees[ c] ees[ aes] |
  g2.-> \bar"|."
}

tenorWordsFirst = \lyricmode {
  \repeat unfold 44 \skip1
  O’er the
}
tenorWords = \lyricmode {
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing;
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
%  Glad -- some the morn -- ing, the land is gay,
%  O’er the mead -- ows,
%  Ech -- oes the brook -- let by wood and lea:
%  “Sing, sing, O heart, be glad with me!”
  
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing,
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
  
  Trip we, oh, so light -- ly, where dew -- y grass is sway -- ing,
  Where ’mid the fair blos -- som the but -- ter -- flies are stray -- ing.
  ’Tis the hour of play -- ing; all voi -- ces are say -- ing:
  “Come, come ye forth a -- May -- ing; to joy a -- wake!”
  Light of Day re -- turn -- eth, glo -- ry of Spring burn -- eth;
  Joy -- notes peal -- ing, gay mu -- sic make.
  Light re -- turn -- eth,
  Glo -- ry of Spring burn -- eth;
  \markup\italic"Tenors humming" _
  \repeat unfold 6 \skip 1
  Gai -- ly is the lark sing -- ing,
  Up -- ward wing -- ing, glad -- ness ring -- ing,
  Un -- to all the mes -- sage bring -- ing:
  “Wel -- come May!”
  Light -- ly trip -- ping, come, oh, come.
  “Sing, O heart! be glad with me!”  
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusicFirst = \relative c' {
  ees,4 g ees |
  f2 bes,4 |
  bes'2 bes,4 |
  ees2 <ees ees,>4 |
  g ees g |
  f2 bes,4 |
  
  bes'2 bes,4 |
  ees2 <ees ees,>4 |
  ees g ees |
  f2 bes,4 |
  bes'2 bes,4 |
  ees2 des4 |
  c e c |
  
  %page2 (83) bass
  f2 f4 |
  bes2 bes,4 |
  ees2. |
  g4-> c-> g-> |
  aes8[ g] f4-. f-. |
  g2-- b,4-. |
  c2. |
  
  r2. r2. |
  bes2-> bes4-. |
  ees2.-> |
  g4-> c-> g-> |
  aes8[ g] f4-. f-. |
  g2-- b,4-. |
  
  %page3 (84) bass
  c2( ees4) |
  f4-. f2-> |
  ees4-. ees2-> |
  d4-. g,2-> |
  c2. |
  ees4 g ees |
  f2 bes,4 |
  
  bes'2 bes,4 |
  ees2 <ees ees,>4 |
  g ees g |
  f2 bes,4 |
  bes'2 bes,4 |
  ees2 <ees ees,>4 |
  ees g ees |
  
  %page4 (85) bass
  f2 bes,4 |
  bes'2 bes,4 |
  ees2 des4 |
  c e c |
  f2 f4 |
  bes2 bes,4 |
  ees2. |
}
bassMusicSecond = \relative c' {
  
  \key aes\major
  aes4-. aes2->~ |
  aes2. |
  ees2.\( |
  aes,\) |
  aes'4-. aes2~ |
  aes2. |
  
  %page5 (86) bass
  bes4\( bes, d |
  ees2.\) |
  aes4-. aes2~ |
  aes2. |
  ees2.\( |
  aes,\) |
  
  aes'4-. aes2~ |
  aes2. |
  bes4~ <bes ees,>2( |
  <aes aes,>2.) |
  f8. g16 aes8[ f] g[ aes] |
  bes8.[ c16] des8[ bes c des] |
  
  %page6 (87) bass
  c8.[ des16] c8 bes aes8[ g] |
  aes16[ bes aes g] f2 |
  r8 f16[ g aes8 f] g[ aes] |
  r8 bes16[ c des8 bes] c[ des] |
  c4 c, e |
  f2. |
  
  r2. |
  <f des>8.-. <f c>16 <f bes,>4-. q-. |
  <c c'>8.-.[ <des des'>16] <c c'>8 <bes bes'> <aes aes'>[ <g g'>] |
  <f f'>2 <g g'>4\fermata |
  aes4.\( bes8 b c |
  
  %page7 (88) bass
  des8[ c] aes4\)-. aes-. |
  bes4-. <ees ees,>2-> |
  aes,4-. aes2-> |
  aes4.\( bes8 b c |
  
  des c aes4-.\) aes-. |
  bes4-> bes-> bes-> |
  <bes ees,>2. |
  aes4-. aes2-> |
  aes'2.( |
  
  %page8 (89) bass
  ees2.) |
  aes,2. |
  aes'4-. aes2->~ |
  aes2. |
  
  bes4-. <bes ees,>2-> |
  <aes aes,>2. |
  bes8-> bes-> bes,4-> bes-> |
  bes'2.-> |
  
  %page9 (90) bass
  bes,2.-> |
  bes2.->\fermata \bar"||"
  aes'4-> c-> aes-> |
  ees2. |
  aes,4-> c-> aes-> |
  <ees ees'>2.->\fermata \bar"|."
}
bassWords = \lyricmode {
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing;
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
   \repeat unfold 35 \skip1

%  Glad -- some the morn -- ing, the land is gay,
%  Trip a -- way;
%  Ech -- oes the brook -- let by wood and lea:
%  “Sing, O heart, be glad with me!”
   \repeat unfold 28 \skip1
  
%  Come ye where gold of May is shin -- ing,
%  Come ye where buds of flow’rs are twin -- ing,
%  As to the bells of fair -- ies chim -- ing,
%  Trip we thro’ bow’rs of ra -- diant Spring.
   \repeat unfold 35 \skip1
  
  \markup\italic"Basses humming" _
  \repeat unfold 15 \skip 1
  Light of Day re -- turn -- eth, glo -- ry of Spring burn -- eth;
  Joy -- notes peal -- ing, gay mu -- sic make.
  Light re -- turn -- eth,
  Glo -- ry of Spring burn -- eth;
  Gai -- ly is the lark sing -- ing,
  Glad -- ness ring -- ing,
  Un -- to all the mes -- sage bring -- ing:
  “Wel -- come the May!
  Wel -- come, Wel -- come, Wel -- come,
  Wel -- come May!”
  Light -- ly trip -- ping, come, oh, come.
  “Sing, sing, O heart! be glad with me!”
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = sopranos <<
      \new Voice = "sopranos" { \global \voiceOne \sopMusicFirst \oneVoice \sopMusicSecond }
      \new Voice = "altosFirst" { \global \voiceTwo \altoMusicFirst }
    >>
    \new Lyrics = "sopranos"
   \new Staff <<
      \new Voice = "altos" { \global \altoMusic }
    >>
    \new Lyrics = "altos"  \lyricsto "altos" \altoWords
    
   \new Staff = tenors <<
      \clef "treble_8"
      \new Voice = "tenors" { \global \tenorMusic }
    >>
    \new Lyrics = "tenors" \lyricsto "tenors" \tenorWords
    \new Staff = basses <<
      \clef bass
      \new Voice = "tenorsFirst" { \global \voiceOne \tenorMusicFirst }
      \new Voice = "basses" { \global \voiceTwo \bassMusicFirst \oneVoice \bassMusicSecond }
    >>
    \new Lyrics = "basses" \lyricsto "basses" \bassWords
    \context Lyrics = "sopranos" \lyricsto "sopranos" \sopWords
    \context Lyrics = "tenors" \lyricsto "tenorsFirst" \tenorWordsFirst
  >>
>>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      %\override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 8)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Song of Spring"}}
  composer = \markup\oldStyleNum"Arthur Pearson (1866–1936)"
  tagline = ""
}}


