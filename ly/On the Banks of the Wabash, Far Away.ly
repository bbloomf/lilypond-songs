\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"On the Banks of the Wabash, Far Away"}}
  composer = \markup\oldStyleNum"Paul Dresser (1858–1906)"
  poet = \markup\oldStyleNum"Paul Dresser (1858–1906)"
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
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.95\in
  outer-margin = 0.7\in
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  c8 des |
  ees ees ees ees \bar"" f8. g16 aes8 f |
  aes4 ees2 \bar"" aes8 bes |
  
  c8 ees c aes \bar"" bes16 a8. bes8 c |
  aes?2~ aes8 b\rest \bar"" c,8 des |
  
  %page2
  ees8. ees16 ees8 ees \bar"" f g aes f |
  aes4 ees2 \bar"" aes8 bes |
  
  c ees c aes \bar"" bes16 a8. bes8 c |
  aes?2~ aes8 b\rest \bar"" aes4 |
  
  aes8 g g g \bar"" c, c aes' g |
  aes4 f2 \bar"" f4 |
  
  %page3
  bes8 bes f g \bar"" aes aes g f |
  g2~ g8 b\rest \bar"" ees,4 |
  
  ees8 ees d ees \bar"" f g aes8. f16 |
  aes4 ees2 \bar"" aes8 bes |
  
  c ees c aes \bar"" bes8. f16 bes8.\fermata aes16 |
  aes2. \bar"||" \break
  
  %page4
  aes8 bes |
  c8. b16 c8. aes16 \bar"" bes8. a16 bes8. g16 |
  aes4 f4. b8\rest \bar"" f8 f |
  
  bes8. c16 bes8 aes \bar"" g8. aes16 g8 f |
  ees2~ ees8 b'\rest \bar"" aes8 bes |
  
  c8. b16 c8. aes16 \bar"" bes8. a16 bes8. g16 |
  aes4 f4.\fermata b8\rest \bar"" f8 fes |
  
  ees4 g8 aes bes16 \bar"" f8.\fermata g8. ees16 |
  aes2~ aes8 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Round my In -- di -- an -- a home -- stead wave the corn -- fields,
  In the dis -- tance loom the wood -- lands clear \set associatedVoice = "altos" and cool,
    \markup\italic clear \markup\italic and \markup\italic cool. \unset associatedVoice
  Of -- ten -- times my thoughts re -- vert to scenes of child -- hood,
  Where I first re -- ceived my les -- sons, \set associatedVoice = "altos" na -- ture’s school,
    \markup\italic na -- \markup\italic ture’s \markup\italic school. \unset associatedVoice
  But one thing there is mis -- sing from the pic -- ture,
  With -- out her face it seems so in -- com -- plete.
  I long to see my moth -- er in the door -- way,
  As she stood there years a -- go, her boy \set associatedVoice = "altos" to greet.
  
  \unset associatedVoice
  Oh, the moon -- light’s fair to -- night a -- long the Wa -- bash,
  From the fields there comes the breath of new -- mown hay. __
  Through the syc -- a --  mores the can -- dle lights are gleam -- ing,
  On the banks of the Wa -- bash, far a -- way. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Man -- y years have passed since I strolled by the riv -- er,
  Arm in arm, with sweet -- heart Mar -- y by \set associatedVoice = "altos" my side,
    \markup\italic by \markup\italic my \markup\italic side. \unset associatedVoice
  It was there I tried to tell her that I loved her,
  It was there I begged of her to be \set associatedVoice = "altos" my bride,
    \set ignoreMelismata = ##t \markup\italic to \markup\italic be \markup\italic my \markup\italic bride. \unset ignoreMelismata \unset associatedVoice
  Long years have passed since I strolled through the church -- yard.
  She’s sleep -- ing there, my an -- gel, Mar -- y dear,
  I loved her, but she thought I did -- n’t mean it,
  Still I’d give my fu -- ture were she \set associatedVoice = "altos" on -- ly here.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 bes |
  c c c c des8. des16 f8 des |
  ees4 ees2 d8 des |
  
  c c ees ees d16 d8. des8 des |
  c4 ees8 des c s c bes |
  
  %page2
  c8. c16 c8 c des des f des |
  ees4 ees2 d8 des |
  
  c c ees ees d16 d8. des8 des |
  c \slurDashed ees( d) \slurSolid des c s c4 |
  
  ees8 ees ees ees c c c e |
  f4 c2 c4 |
  
  %page3
  bes8 bes bes bes d d d d |
  ees2~ ees8 s ees4 |
  
  c8 c b c des des f8. des16 |
  ees4 ees2 d8 des |
  
  c c ees ees d8. d16 des8. c16 |
  c4( des c) \bar"||"
  
  %page4/chorus
  c8 des |
  ees8. ees16 ees8. ees16 e8. e16 e8. e16 |
  f4 des4. s8 des8 ees |
  
  d8. d16 d8 d d?8. d16 d8 d |
  bes4( a bes8) s c des |
  
  ees8. ees16 ees8. ees16 e8. e16 e8. e16 |
  f4 des4. s8 des8 b |
  
  c4 c8 ees d16 d8. des8. des16 |
  c4( des c8) s \bar"|."
}
altoWords = \lyricmode {
  \repeat unfold 49 ""
  na -- ture’s school.
}
altoWordsII = \lyricmode {
%\markup\italic
  \repeat unfold 49 ""
  \set ignoreMelismata = ##t
  to be my bride.
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
  aes8 bes |
  aes aes aes aes aes8. aes16 aes8 aes |
  c4 c2 aes8 aes |
  
  aes aes c aes bes16 f8. g8 g |
  ees4 g8 f ees s aes bes |
  
  %page2
  aes8. aes16 aes8 aes aes aes aes aes |
  c4 c2 aes8 aes |
  
  aes aes c aes bes16 f8. g8 g |
  \slurDashed ees8 ges( f) \slurSolid fes ees s ees4 |
  
  c'8 c c c g g aes bes |
  c4 aes2 aes4 |
  
  %page3
  aes8 aes aes aes bes bes bes aes |
  bes2~ bes8 s g4 |
  
  aes8 aes aes aes aes aes aes8. aes16 |
  c4 c2 aes8 aes |
  
  aes aes c aes aes8. aes16 g8. aes16 |
  ees4( f ees) \bar"||"
  
  %page4/chorus
  aes8 aes |
  c8. c16 c8. c16 c8. c16 c8. bes16 |
  aes4 aes4. s8 aes a |
  
  bes8. bes16 aes8 aes bes8. aes16 bes8 aes |
  g4( fis g8) s aes aes |
  
  c8. c16 c8. c16 c8. c16 c8. bes16 |
  aes4 aes4. s8 aes aes |
  
  aes4 g8 aes f16 f8. bes8. g16 |
  ees4( fes ees8) s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,8 aes |
  aes aes aes aes des8. des16 des8 des |
  aes4 aes2 f'8 fes |
  
  ees c aes c f16 f8. ees8 ees |
  aes,2~ aes8 d\rest aes aes |
  
  %page2
  aes8. aes16 aes8 aes des des des des |
  aes4 aes2 f'8 fes |
  
  ees c aes c f16 f8. ees8 ees |
  aes,2~ aes8 d\rest aes4 |
  
  c8 c c c e e e e |
  f4 f2 f8[ ees] |
  
  %page3
  d d d d bes bes bes bes |
  ees2~ ees8 d\rest ees4 |
  
  aes,8 aes aes aes des des des8. des16 |
  aes4 aes2 f'8 fes |
  
  ees8 c aes c bes8. bes16 ees8. ees16 |
  aes,2. \bar"||"
  
  %page4/chorus
  aes8 aes |
  aes8. aes16 aes8. aes16 g8. g16 g8. c16 |
  f,4 des'4. d8\rest des c |
  
  bes8. bes16 bes8 bes bes8. bes16 bes8 bes |
  ees2~ ees8 d\rest aes8 aes |
  
  aes8. aes16 aes8. aes16 g8. g16 g8. c16 |
  f,4 des'4. d8\rest des d |
  
  ees4 ees8 c bes16 bes8.\fermata ees8. ees16 |
  aes,2~ aes8 d\rest \bar"|."
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
    %\new Lyrics = "trueAltos"
    %\new Lyrics = "trueAltosII"
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
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
