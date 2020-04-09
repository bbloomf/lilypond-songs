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
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 3)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #36
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  \set midiInstrument = #"flute"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \set midiInstrument = #"acoustic grand"
  <ees bes>4 <ees c>8.[ <f d>16] << {g8.[ f16] g16[ bes8.] } \\ {ees,4 d} >> |
  <c ees aes c>4->^\markup\italic"poco rit." <ees aes ees'>-> <g bes ees g>2->\fermata
  \set midiInstrument = #"flute"
  ees4 ees8. f16 g8. f16 g bes8. |
  
  ees,4 ees8. aes16 g8. bes16 f4 |
  ees ees8. f16 g8. f16 g bes8. |
  c8 ees bes ees g, ees' f,4 |
  
  g8 bes bes c16[ d] ees8 bes c bes |
  g bes bes c16[ d] ees8 g, f4 |
  
  g8 bes bes c16[ d] ees8 bes c bes |
  c8 ees bes8 ees g, ees' f,4 \bar"||"
  
  ees4 ees8. f16 g8. f16 g bes8. |
  ees,4 ees8. aes16 g8. bes16 f4 |
  
  ees ees8. f16 g8. f16 g bes8. |
  c8 ees bes8 g f8. g16 ees4\fermata \bar"|."
  
}
sopWords = \lyricmode {
  \repeat unfold 5 \skip1
  \set stanza = #"1. "
  Who would not fight for Free -- dom?
  Who would not draw the sword?
  Who would not up and ral -- ly
  At the great Re -- pub -- lic’s word?
  It -- a -- ly’s fair plains are rav -- aged,
  Ven -- ice threat -- en’d by the Hun,
  Quick -- ly let us cross the o -- cean
  Ere the cru -- el deed is done.
}

sopWordsII = \lyricmode {
  \set stanza = \markup\dynamic"f"
  \repeat unfold 5 \skip1
  \set stanza = #"2. "
  Who would not fight for Bel -- gium?
  Who would not fight for France?
  Who would not stand with Eng -- land
  To re -- pel the foe’s ad -- vance?
  We have heard their wo -- men call -- ing
  For our help a -- cross the sea,
  We have heard their weep -- ing chil -- dren;
  Come and fight and set them free.
  
  Who would not fight for Free -- dom?
  Who would not draw the sword?
  Who would not up and ral -- ly
  At the great Re -- pub -- lic’s word?
}

sopWordsIII = \lyricmode {
  \repeat unfold 5 \skip1
  \set stanza = #"3. "
  Who would not fight the Prus -- sian?
  What man would be a slave?
  Up, then, let ev -- ’ry free -- man
  Fight, his coun -- try’s life to save.
  Ev -- ’ry man whose heart is loy -- al,
  Ev -- ’ry man of cour -- age tried,
  Let him heed his coun -- try’s sum -- mons,
  Let him stand on Free -- dom’s side.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  s1*2 |
  bes4 c8. d16 ees8. ees16 d16 d8. |
  
  c4 bes8. d16 ees8. ees16 d4 |
  bes4 c8. d16 ees8. ees16 d16 g8. |
  aes8 aes ees ees ees ees d4 |
  
  ees8 ees ees g g g ees ees |
  ees8 ees ees g g ees d4 |
  
  ees8 ees ees g g g ees ees |
  aes8 aes ees8 ees ees ees d4 |
  
  bes4 c8. d16 ees8. ees16 d16 d8. |
  c4 bes8. d16 ees8. ees16 d4 |
  
  bes4 c8. d16 ees8. d16 ees16 g8. |
  aes8 aes g8 ees c d bes4 \bar"|."
}
altoWords = \lyricmode {
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
tenorMusic = \relative c' {
  s1*2 |
  g4 g8. bes16 bes8. aes16 bes bes8. |
  
  aes4 bes8. bes16 bes8. g16 bes4 |
  g g8. bes16 bes8. aes16 bes d8. |
  ees8 c bes bes bes g bes4 |
  
  bes8 g g bes bes bes g g |
  bes g g bes bes bes bes4 |
  
  bes8 g g bes bes bes g g |
  ees'8 c bes bes bes g bes4 |
  
  g4 g8. bes16 bes8. aes16 bes bes8. |
  aes4 bes8. bes16 bes8. g16 bes4 |
  
  g g8. bes16 bes8. bes16 bes ees8. |
  ees8 c ees bes aes f g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \override DynamicLineSpanner #'Y-extent = #'(-6 . 0)
  \set midiInstrument = #"acoustic grand"
  <g ees>4 <g c,>8.[ <bes bes,>16] <bes ees,>4 <bes g> |
  <aes aes,>4->\< <c aes>-> <bes ees,>2->\fermata\!
  \set midiInstrument = #"flute"
  ees,4 c8. bes16 ees8. ees16 g g8. |
  
  aes4 g8. f16 ees8. ees16 bes4 |
  ees c8. bes16 ees8. ees16 g g8. |
  aes8 aes g g ees ees bes4 |
  
  ees8 ees ees ees16[ d] ees8 ees c ees 
  ees8 ees ees ees16[ d] ees8 g, bes4 |
  
  ees8 ees ees ees16[ d] ees8 ees c ees |
  aes8 aes g g ees ees bes4 \bar"||"
  
  ees4 c8. bes16 ees8. ees16 g g8. |
  aes4 g8. f16 ees8. ees16 bes4 |
  
  ees c8. bes16 ees8. aes16 g ees8. |
  aes8 aes g g, aes bes ees4 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
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
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Who would not fight for freedom?"}}
  composer = \markup\oldStyleNum"Old Scotch Air"
  tagline = ""
}}
global = {
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
	\partial 4 g8[ f] |
  d4 g g a |
  bes2 bes4 c8[ bes] |
  a4. g8 f4 e |
  f2 bes4\rest g8[ f] |
  
  d4 g g a |
  bes2 bes4 c |
  d4. c8 bes4 c |
  d2 bes4\rest f'4 |
  d4. c8 bes4 d |
  
  f2 d4 d |
  c4. bes8 a4 bes |
  c2 bes4\rest bes8[ c] |
  d4 bes c a |
  bes g d'\fermata g,8[ f] |
  
  d4 g g f |
  g2. \bar "||"
  
	g8[ f] |
  d4 g g a |
  bes2 bes4 c8[ bes] |
  a4. g8 f4 e |
  f8 f4. bes4\rest g8[ f] |
  
  d4 g g a |
  bes2 bes4 c |
  d4. c8 bes4 c |
  d8 d4. bes4\rest f'4 |
  d4. c8 bes4 d |
  
  f2 d4 d |
  c4. bes8 a4 bes |
  c2 bes4\rest bes8 c |
  d4 bes c a |
  bes g d'\fermata g,8[^\markup\italic"più adagio" f] |
  
  d4 g g f |
  g2. \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	John An -- der -- son, my jo, John,
  When we were first ac -- quent,
  Your locks were like the ra -- ven,
  Your bon -- nie brow was brent;
  But now your brow is bald, John,
  Your locks are like the snow,
  Yet, bless -- ings on your frost -- y pow,
  John An -- der -- son, my jo.
  
  \set stanza = #"2. "
	John An -- der -- son, my jo, John,
  We clamb the hill to -- gith -- er;
  And mon -- ie~a cant -- y day, John,
  We’ve had wi’ ane an -- ith -- er.
  Now we maun tot -- ter down, John,
  But hand in hand we’ll go,
  And we’ll sleep to -- gith -- er at the foot,
  John An -- der -- son, my jo.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

altoMusic = \relative c' {
  \partial 4 g'8[ f] |
  d4 g g f |
  f2 f4 g |
  f4. e8 c4 c |
  c2 s4 c4 |
  
  d4 d d f |
  f2 f4 a |
  bes4. a8 f4 a |
  bes2 s4 d |
  bes4. f8 d4 bes' |
  
  d2 bes4 bes |
  a4. f8 f4 f |
  f2 s4 f8[ a] |
  bes4 f f f |
  d g fis d8[ c] |
  
  d4 d d d |
  d2. \bar "||"
  
  g8[ f] |
  d4 g g f |
  f2 f4 g |
  f4. e8 c4 c |
  c8 c4. s4 c4 |
  
  d4 d d f |
  f2 f4 a |
  bes4. a8 f4 a |
  bes8 bes4. s4 d |
  bes4. f8 d4 bes' |
  
  d2 bes4 bes |
  a4. f8 f4 f |
  f2 s4 f8 a |
  bes4 f f f |
  d g fis d8[ c] |
  
  d4 d d d |
  d2. \bar "|."
}
altoWords = \lyricmode {
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
tenorMusic = \relative c' {
  \partial 4 g8[ f] |
  d4 g g c |
  d2 d4 d |
  c4. c8 a4 g |
  a2 s4 c |
  
  bes4 bes bes c |
  d2 f4 f |
  f4. f8 f4 f |
  f2 s2 |
  s1 |
  
  s2. f4 |
  f4. d8 c4 d |
  a2 s4 d8[ c] |
  bes4 bes a c |
  bes d a bes8[ c] |
  
  d4 bes g a |
  bes2. \bar "||"
  
  g8[ f] |
  d4 g g c |
  d2 d4 d |
  c4. c8 a4 g |
  a8 a4. s4 c |
  
  bes4 bes bes c |
  d2 f4 f |
  f4. f8 f4 f |
  f8 f4. s2 |
  s1 |
  
  s2. f4 |
  f4. d8 c4 d |
  a2 s4 d8 c |
  bes4 bes a c |
  bes d a bes8[ c] |
  
  d4 bes g a |
  bes2. \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4 g8[ f] |
  d4 g g f |
  bes,2 bes'4 g |
  c,4. c8 c4 c |
  f2 d4\rest a4 |
  
  bes d g f |
  bes,2 d4 f |
  bes4. c8 d4 c |
  bes2 d,2\rest |
  d1\rest |
  
  d2\rest d4\rest bes'4 |
  f4. f8 f4 f |
  f2 d4\rest f |
  bes,4 d f f |
  g bes d,_\fermata bes8[ a] |
  
  bes4 g bes d |
  g,2. \bar "||"
  
  g'8[ f] |
  d4 g g f |
  bes,2 bes'4 g |
  c,4. c8 c4 c |
  f8 f4. d4\rest a4 |
  
  bes d g f |
  bes,2 d4 f |
  bes4. c8 d4 c |
  bes8 bes4. d,2\rest |
  d1\rest |
  
  d2\rest d4\rest bes'4 |
  f4. f8 f4 f |
  f2 d4\rest f8 f |
  bes,4 d f f |
  g bes d,_\fermata bes8[ a] |
  
  bes4 g bes d |
  g,2. \bar "|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
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
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"John Anderson, my jo"}}
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum"Harmonized by Max Vogrich (1852–1916)"
  tagline = ""
}}


