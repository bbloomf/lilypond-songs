\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Rule Britannia"}}
  poet = \markup\oldStyleNum"James Thomson (1700–1748)"
  composer = \markup\oldStyleNum"Thomas Arne (1710–1778)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key a \major
  \time 2/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  e4 |
  a2 a |
  a8[ b cis d]( e4) a, |
  b2. cis8[ d] |
  cis2 b4\rest e, |
  a8[ b a b]( cis)[ d cis d]( |
  e4) b cis b |
  
  a4( b8)[ cis]( b4) a |
  gis2. e4 |
  gis e b' gis |
  e' dis8[ cis] b[ a] gis[ fis] |
  e2 gis4.( fis8) |
  e2 b'2\rest |
  
  a2 a4. e8 |
  fis4 d b'\rest a |
  d4. cis8 b4. a8 |
  a4( gis2) e4 |
  e'2 d |
  cis8[ a d b]( e4) d |
  cis2( b4.) a8 | a2 b\rest | \break
  
  \repeat volta 2 {
    cis2 b4\rest cis |
    d d b\rest cis |
    d4. cis8 b4. a8 |
    gis1 |
    e'2 d |
    cis8[ a d b]( e4) d |
    cis2 b |
    a2 b\rest |
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	When Brit -- ain first __ at Heav’n’s com -- mand,
  A -- rose __ from out the a -- zure main,
  A -- rose, a -- rose, a -- rose from out the a -- zure main,
  This was the char -- ter, the char -- ter of the land,
  And gaurd -- ian an -- gels sang this strain:
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The na -- tions not __ so blest as thee,
  Shall in __ their turn to ty -- rants bend,
  Shall in their turn, shall in their turn to ty -- rants bend.
  While thou shalt flour -- ish, shalt flour -- ish great and free,
  And to the weak __ pro -- tec -- tion lend.
  
  Rule, Bri -- tan -- nia, Bri -- tan -- nia rule the waves!
  Brit -- ons nev -- er shall be slaves.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  To thee be -- longs __ the ru -- ral reign,
  Thy cit -- ies shall with com -- merce shine,
  Thy cit -- ies shall with com -- merce, shall with com -- merce shine,
  And lands far o -- ver, far o’er the spread -- ing main,
  Shall stretch a hand __ to grasp with thine.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e4 |
  cis2 d |
  cis8[ e a b]( cis4) a |
  gis2. a8[ b] |
  a2 s4 e |
  cis8[ e cis e]( a)[ b a b]( |
  cis4) gis a gis |
  
  fis2 dis |
  e2. e4 |
  gis e e e |
  gis b8[ a] gis[ fis] e4 |
  e2 dis |
  e2 s |
  
  cis e4. cis8 |
  d4 d s fis |
  fis4. fis8 fis4. fis8 |
  e2. e4 |
  e2 gis |
  a8[ e d fis]( e4) fis |
  e2( gis4.) a8 |
  a2 s |
  
  \repeat volta 2 {
    a2 s4 g |
    fis fis s a |
    b4. a8 gis4. fis8 |
    e1 |
    a2 b |
    a8[ e fis d]( e4) fis |
    e2 e4( d) |
    cis2 s |
  }
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
  e,4 |
  e2 fis |
  a8[ gis a gis]( a4) a |
  e'2 e |
  e s4 e, |
  a8[ gis a gis]( e'2)~ |
  e4 e e d |
  
  cis2 fis,4( b) |
  b2. gis4 |
  gis e gis b |
  b a cis a |
  gis2 b4.( a8) |
  gis2 s |
  
  e a4. a8 |
  a4 fis s a |
  b4. ais8 b4. b8 |
  b2. gis4 |
  a2 e |
  e4( b' a) a |
  a2( d4.) cis8 |
  cis2 s |
  
  \repeat volta 2 {
    e2 s4 e |
    d a s e' |
    e4. e8 e4. b8 |
    e1 |
    cis2 e |
    a,2. a4 |
    a2 gis |
    a2 s |
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 |
  a,2 d |
  a8[ e' a e]( a,4) cis |
  e2 e |
  a2 d,4\rest e |
  a,8[ e' a e]( a)[ e a gis]( |
  a4) b a e |
  
  fis2 b, |
  e2. e4 |
  gis e gis e |
  e, a a a |
  b2 b |
  e2 d\rest |
  
  a a4. a8 |
  d4 d d\rest d |
  b4. cis8 d4. dis8 |
  e2. d4 |
  cis2 b |
  a4( b cis) d |
  e2~ e4. a,8 |
  a2 d\rest |
  
  \repeat volta 2 {
    a' d,4\rest a' |
    d, d d\rest a' |
    gis4. a8 e4. dis8 |
    e1 |
    a2 gis |
    a4( d, cis) d |
    e2 e |
    a,2 d\rest |
  }
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
}

\score {
  \unfoldRepeats
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
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}


