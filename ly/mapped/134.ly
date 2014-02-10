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
       (padding . 20)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #134
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
  evenFooterMarkup = \markup\oldStyleNum"*An ancient name for Ireland.    †A town in North eastern Ireland, once the chief seat of the Kings of Ulster."
  oddFooterMarkup = ""
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key f \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 8
  c8 |
  f8. g16 f8 f g a |
  c8. d16 c8 c a f |
  g8. f16 g8 \bar""
  
  a8 f c |
  d8. e16 d8 d4 c16~ c |
  f8. g16 f8 f8. g16 a8 |
  
  c8. d16 c8 c a f |
  g8. f16 g8 a f c |
  d4 e8 f4 c'8 |
  
  c d e f4 d8 |
  e4 c8 d4 a8 |
  c d e f e d |
  
  %page2
  e8 d c d4\fermata c16[ c] |
  c8 d e f4 d8 |
  e d c d4 e16[ f] |
  
  f,8. g16 f8 f8. g16 a8 |
  c8. d16 c8 c8. a16 f8 |
  g8. f16 g8 \bar""
  
  a8 f c |
  d8. e16 d8 d4 c8 |
  f8. g16 f8 f8. g16 a8 |
  
  c8. d16 c8 c8. a16 f8 |
  g8. f16 g8 a f c |
  d4 e8 f4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh! blest be the days when the green ban -- ner float -- ed,
  Sub -- lime o’er the moun -- tains of free In -- nis -- fail,*
  \set ignoreMelismata = ##t
  When her sons to her glo -- ry and free -- dom de -- vot -- ed,
  \unset ignoreMelismata
  De -- fied the in -- vad -- er to tread her soil,
  When back o’er the main they chased the Dane,
  And gave to re -- li -- gion and learn -- ing their spoil,
  When val -- or and mind to -- geth -- er com -- bined.
  But where -- fore la -- ment o’er the glo -- ries de -- part -- ed,
  Her stars shall shine out with as viv -- id a ray;
  For ne’er had she chil -- dren more brave and true heart -- ed,
  Than those she sees now on Saint Pat -- rick’s Day.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Her scep -- ter, a -- las! passed a -- way to the stran -- ger;
  And trea -- son sur -- ren -- dered what val -- or hath held;
  But __ true hearts re -- mained a -- mid dark -- ness and dan -- ger,
  Which ’spite of her ty -- rants would not be quelled.
  Oft, oft, through the night flashed gleams of light
  Which al -- most the dark -- ness of bond -- age dis -- pelled;
  \set ignoreMelismata = ##t
  But a star now is near, her heav -- en to cheer,
  Not _ like the wild gleams which so fit -- ful -- ly dart -- ed,
  But long to shine down with its hal -- low -- ing ray
  On daugh -- ters as fair, and on sons as true heart -- ed,
  As Er -- in be -- holds on Saint Pat -- rick’s Day.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Oh! blest be the hour, when be -- girt by her can -- non,
  And hailed as it rose by a na -- tion’s ap -- plause,
  That __ flag waved a -- loft o’er the spires of Dun -- gan -- non,†
  As -- sert -- ing for I -- rish -- men, I -- rish laws.
  Once more it shall wave o’er hearts as brave,
  De -- spite of the das -- tards who mock at her cause,
  \set ignoreMelismata = ##t
  And like broth -- ers a -- greed, what -- ev -- er their creed,
  Her _ chil -- dren in -- spired by those glo -- ries de -- part -- ed,
  No lon -- ger in dark -- ness de -- spond -- ing will stay,
  But join in her cause like the brave and true heart -- ed
  Who rise for their rights on Saint Pat -- rick’s Day.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 |
  c8. c16 c8 c c f |
  f8. f16 f8 f f f |
  e8. d16 e8
  
  f8 c a 
  bes8. bes16 bes8 bes4 bes16~ bes |
  a8. a16 a8 c8. c16 f8
  
  f8. f16 f8 f f f 
  e8. d16 e8 f c a 
  bes4 bes8 a4 f'8 |
  
  e8 f g  f4 a8 
  g4 g8 f4 f8 
  e f g f f f |
  
  %page 2 alto
  e8 e g f[ d]^\fermata f16[ f] |
  e8 f g  f4 a8 |
  g g g8 f4 c8 |
  
  a8. a16 a8 c8. c16 f8 
  f8. f16 f8 f8. f16 f8
  e8. d16 e8
  
  f8 c a 
  bes8. bes16 bes8 bes4 bes8
  a8. a16 a8 c8. c16 f8
  
  f8. f16 f8 f8. f16 f8
  e8. d16 e8 f c a 
  bes4 bes8 a4 \bar"|."
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
  a8 |
  a8. bes16 a8 a bes c 
  a8. bes16 a8 a c a 
  c8. c16 c8
  
  c a f 
  f8. f16 f8 f4 e16( g) |
  f8. f16 f8 a8. bes16 c8
  
  a8. bes16 a8 a c a 
  c8. c16 c8 c a f 
  f4 c8 f4 a8 |
  
  g8 g c a4 a8 |
  bes4 bes8 a4 a8 |
  bes bes bes a a a |
  
  %page2 tenor
  bes8 bes bes a4 a16[ a] |
  g8 g c a4 a8 |
  bes bes bes8 a4 bes16[ a] |
  
  f8. f16 f8 a8. bes16 c8
  a8. bes16 a8 a8. c16 a8
  c8. c16 c8
  
  c a f
  f8. f16 f8 f4 e8
  f8. f16 f8 a8. bes16 c8
  
  a8. bes16 a8 a8. c16 a8
  c8. c16 c8 c a f
  f4 c8 f4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 |
  f8. f16 f8 f f f 
  f8. f16 f8 f f f 
  c8. c16 c8
  
  f8 f f
  bes,8. bes16 bes8 bes4 c16( e) |
  f8. f16 f8 f8. f16 f8
  
  f8. f16 f8 f f f
  c8. c16 c8 f f f
  bes,4 c8 f,4 f8 |
  
  c' c c d4 f8 |
  g4 e8 f4 f8 |
  c c c d d d |
  
  %page2 bass
  g8 g e f4\fermata f16[ f] |
  c8 c c d4 f8 |
  g g e8 f4 g16[ f] |
  
  f,8. f16 f8 f'8. f16 f8
  f8. f16 f8 f8. f16 f8
  c8. c16 c8
  
  f f f
  bes,8. bes16 bes8 bes4 c8 |
  f,8. f16 f8 f'8. f16 f8 |
  
  f8. f16 f8 f8. f16 f8 |
  c8. c16 c8 f f f |
  bes,4 c8 f,4 \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Saint Patrick’s Day"}}
  poet = \markup\oldStyleNum"M. J. Barry"
  composer = \markup\oldStyleNum"Irish Folk Song"
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

sopMusic = \relative c' {
	\partial 4
  a'4 |
  f4. g8 a4 a |
  f4. g8 a4 bes |
  a g f e |
  f2. \bar"||"
}
sopWords = \lyricmode {
  The bell doth toll,
  Its ech -- oes roll,
  I know the sound full well;
}

sopWordsII = \lyricmode {
  I love its ring -- ing
  For it calls to sing -- ing
  With its bim, bim, bim, bom bell,
}

sopWordsIII = \lyricmode {
  Bim,
  Bom,
  Bim, bim, bim, bom bell.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c'4 |
  a4. bes8 c c c c |
  a4. bes8 c c d d |
  c4 bes a g |
  a2. \bar"||"
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
  b'4\rest |
  f2. b4\rest |
  f2. b4\rest |
  c c c, c |
  f2. \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
    \new Staff = women <<
      \new Voice = "sopranos" { << \global \sopMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Staff = women <<
      \new Voice = "altos" { << \global \altoMusic >> }
    >>
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Staff = women <<
      \new Voice = "tenors" { << \global \tenorMusic >> }
    >>
    \new Lyrics = "altosIII"  \lyricsto "tenors" \sopWordsIII
    
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The bell doth toll"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"(Round)"}}
  tagline = ""
}}


